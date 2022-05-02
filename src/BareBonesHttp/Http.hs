{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module BareBonesHttp.Http
  ( module BareBonesHttp.Http.Definitions,
    module BareBonesHttp.Http.AsResponse,
    module BareBonesHttp.Http.Capabilities,
    module BareBonesHttp.Http.RouteHandler,
    RequestMonad (..),
    RequestMonadT (..),
    MiddleWare,
    serveFiles,
    handleServer,
    runServer,
    handleServerRoutes,
    runServerRoutes,
  )
where

import BareBonesHttp.Bidi
import BareBonesHttp.Http.AsResponse
import BareBonesHttp.Http.Capabilities
import BareBonesHttp.Http.Definitions
import qualified BareBonesHttp.Http.Internal.Connections as C
import BareBonesHttp.Http.RouteHandler
import Control.Arrow
import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader.Class
import qualified Control.Monad.Trans.Except as MExcept
import qualified Control.Monad.Trans.RWS as MRWS
import qualified Control.Monad.Trans.Reader as MReader
import qualified Control.Monad.Trans.State as MState
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import Data.List (intercalate)
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Network.Run.TCP
import Network.Socket
import System.Directory (doesFileExist)

-------------------- Request & Response --------------------

headersToMap :: [C.Header] -> Headers
headersToMap headers =
  let withCookies = Map.fromListWith (\a b -> a <> ", " <> b) (fmap (\(C.Header k v) -> (k, v)) headers)
   in Map.delete "Set-Cookie" (Map.delete "Cookie" withCookies)

headersFromMap :: Headers -> [C.Header]
headersFromMap headers = fmap (uncurry C.Header) (Map.toList headers)

data CookieType = GetCookie | SetCookie

cookieTypeToHeaderName :: CookieType -> CI.CI T.Text
cookieTypeToHeaderName GetCookie = "Cookie"
cookieTypeToHeaderName SetCookie = "Set-Cookie"

cookiesToMap :: CookieType -> [C.Header] -> Map.Map T.Text T.Text
cookiesToMap cookieType headers = Map.delete "" $ Map.fromList cookieHeaders
  where
    cookieContents :: [T.Text]
    cookieContents = C._headerContent <$> filter (\(C.Header n _) -> n == cookieTypeToHeaderName cookieType) headers

    cookieHeaders :: [(T.Text, T.Text)]
    cookieHeaders = case cookieType of
      GetCookie -> (cookieContents >>= T.splitOn "; ") <&> T.span (== '=')
      SetCookie -> cookieContents <&> T.span (== '=')

cookiesFromMap :: CookieType -> Map.Map T.Text T.Text -> [C.Header]
cookiesFromMap cookieType cookies =
  (\(a, b) -> C.Header (cookieTypeToHeaderName cookieType) (a <> "=" <> b)) <$> Map.toList cookies

requestInfoToRequest :: C.RequestInfo -> Request ()
requestInfoToRequest (C.RequestInfo method uri _ headers) =
  Request method uri (headersToMap headers) (cookiesToMap GetCookie headers) ()

{-
requestToRequestInfo :: ProtocolVersion -> Request c -> C.RequestInfo
requestToRequestInfo protocol (Request method uri headers cookies _) =
  C.RequestInfo method uri protocol (headersFromMap headers ++ cookiesFromMap GetCookie cookies)
-}

responseInfoToResponse :: C.ResponseInfo -> Response ()
responseInfoToResponse (C.ResponseInfo _ status headers body) =
  Response status (headersToMap headers) (cookiesToMap SetCookie headers) body ()

responseToResponseInfo :: ProtocolVersion -> Response c -> C.ResponseInfo
responseToResponseInfo protocol (Response status headers cookies body _) =
  C.ResponseInfo protocol status (headersFromMap headers ++ cookiesFromMap SetCookie cookies) body

transformRequestResponse :: (Arrow a) => Bidi a C.RequestInfo (Request ()) (Response ()) C.ResponseInfo
transformRequestResponse = Bidi (arr requestInfoToRequest) (arr (responseToResponseInfo Http11))

-------------------- RequestMonad --------------------

class (Monad m) => RequestMonad m where
  requestInfo :: m C.RequestInfo

  request :: m (Request ())
  request = requestInfoToRequest <$> requestInfo

  requestBody :: m (Maybe (B.ByteString, Headers))

  closeAfterDone :: m Bool

  setCloseAfterDone :: m ()

newtype RequestMonadT m a = RequestMonad
  { runRequest ::
      MExcept.ExceptT
        (Response ())
        ( MRWS.RWST
            (C.RequestInfo, Maybe (RequestMonadT m (B.ByteString, Headers)))
            ()
            C.ConnectionState
            m
        )
        a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError (Response ()), MonadLogger)

connectionToRequestMonad :: (Functor m) => C.ConnectionMonad m a -> RequestMonadT m a
connectionToRequestMonad (C.ConnectionMonad (MExcept.ExceptT (MState.StateT ma))) =
  let ma' c = (\(a, b) -> (a, b, ())) <$> ma c
   in RequestMonad
        ( MExcept.withExceptT
            responseInfoToResponse
            (MExcept.ExceptT (MRWS.RWST (const ma')))
        )

requestToConnectionMonad :: (Functor m) => C.RequestInfo -> Maybe (RequestMonadT m (B.ByteString, [C.Header])) -> RequestMonadT m a -> C.ConnectionMonad m a
requestToConnectionMonad req body (RequestMonad (MExcept.ExceptT (MRWS.RWST ma))) =
  let r' = (req, (fmap . fmap) (over _2 headersToMap) body)
   in let ma' con = (fmap . fmap) (\(a, b, _) -> (a, b)) (ma r') con
       in C.ConnectionMonad
            ( MExcept.withExceptT
                (responseToResponseInfo Http11)
                (MExcept.ExceptT (MState.StateT ma'))
            )

instance (Monad m) => RequestMonad (RequestMonadT m) where
  requestInfo = RequestMonad $ asks fst

  requestBody = RequestMonad (asks snd) >>= sequence

  closeAfterDone = connectionToRequestMonad $ use C.closeWhenDone

  setCloseAfterDone = connectionToRequestMonad (C.closeWhenDone .= True)

-------------------- General layers --------------------

ensureCloseCorrect :: (RequestMonad m) => Response c -> m (Response c)
ensureCloseCorrect response = do
  isHttp10 <- (Http10 ==) . C._requestProtocol <$> requestInfo
  settingShouldCloseWhenDone <- closeAfterDone
  connectionHasClose <- anyOf (requestHeaders . ix "Connection") (== "close") <$> request
  let closing = isHttp10 || settingShouldCloseWhenDone || connectionHasClose
  unless settingShouldCloseWhenDone setCloseAfterDone
  let newResponse =
        if not closing || connectionHasClose
          then response
          else set (responseHeaders . ix "Connection") "close" response
  pure newResponse

ensureBodyRead :: (RequestMonad m) => Response c -> m (Response c)
ensureBodyRead response = do
  closeDone <- closeAfterDone
  unless closeDone (() <$ requestBody)
  pure response

setBodyContentLength :: Response c -> Response c
setBodyContentLength r@(Response _ _ _ (Just body) _) =
  set (responseHeaders . ix "Content-Length") (T.pack $ show $ B.length body) r
setBodyContentLength r = r

setCorrectnessHeaders :: (RequestMonad m) => MiddleWare m c1 c2
setCorrectnessHeaders = Bidi (arr id) (Kleisli ensureCloseCorrect >>> Kleisli ensureBodyRead >>^ setBodyContentLength)

-------------------- Server functions --------------------

serveFiles :: (MonadIO m, MonadError (Response ()) m, MonadLogger m) => String -> Request c -> [T.Text] -> m (Response c)
serveFiles basePath req path = do
  let pathHasGetParent = ".." `elem` path
  when pathHasGetParent (throwError $ Response Forbidden Map.empty Map.empty Nothing ())
  let filePath = basePath ++ intercalate "/" (fmap T.unpack path)
  $(logInfo) ("Trying to send file " <> T.pack filePath)
  fileExists <- liftIO (doesFileExist filePath)
  unless fileExists (throwError $ Response NotFound Map.empty Map.empty Nothing ())
  contents <- liftIO (B.readFile filePath)
  let fileExtension = snd (T.breakOnEnd "." (last path))
  let contentTypeHeader = maybe Map.empty (Map.singleton "Content-Type") (contentTypeFromFileExtension fileExtension)
  pure $ Response Ok contentTypeHeader Map.empty (Just contents) (_requestCap req)

--https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
contentTypeFromFileExtension :: T.Text -> Maybe T.Text
contentTypeFromFileExtension "aac" = Just "audio/aac"
contentTypeFromFileExtension "abw" = Just "application/x-abiword"
contentTypeFromFileExtension "arc" = Just "application/x-freearc"
contentTypeFromFileExtension "avif" = Just "image/avif"
contentTypeFromFileExtension "avi" = Just "video/x-msvideo"
contentTypeFromFileExtension "azw" = Just "application/vnd.amazon.ebook"
contentTypeFromFileExtension "bin" = Just "application/octet-stream"
contentTypeFromFileExtension "bmp" = Just "image/bmp"
contentTypeFromFileExtension "bz" = Just "application/x-bzip"
contentTypeFromFileExtension "bz2" = Just "application/x-bzip2"
contentTypeFromFileExtension "cda" = Just "application/x-cdf"
contentTypeFromFileExtension "csh" = Just "application/x-csh"
contentTypeFromFileExtension "css" = Just "text/css"
contentTypeFromFileExtension "csv" = Just "text/csv"
contentTypeFromFileExtension "doc" = Just "application/msword"
contentTypeFromFileExtension "docx" = Just "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
contentTypeFromFileExtension "eot" = Just "application/vnd.ms-fontobject"
contentTypeFromFileExtension "epub" = Just "application/epub+zip"
contentTypeFromFileExtension "gz" = Just "application/gzip"
contentTypeFromFileExtension "gif" = Just "image/gif"
contentTypeFromFileExtension "htm" = Just "text/html"
contentTypeFromFileExtension "html" = Just "text/html"
contentTypeFromFileExtension "ico" = Just "image/vnd.microsoft.icon"
contentTypeFromFileExtension "ics" = Just "text/calendar"
contentTypeFromFileExtension "jar" = Just "application/java-archive"
contentTypeFromFileExtension "jpeg" = Just "image/jpeg"
contentTypeFromFileExtension "jpg" = Just "image/jpeg"
contentTypeFromFileExtension "js" = Just "text/javascript"
contentTypeFromFileExtension "json" = Just "application/json"
contentTypeFromFileExtension "jsonld" = Just "application/ld+json"
contentTypeFromFileExtension "mid" = Just "audio/midi audio/x-midi"
contentTypeFromFileExtension "midi" = Just "audio/midi audio/x-midi"
contentTypeFromFileExtension "mjs" = Just "text/javascript"
contentTypeFromFileExtension "mp3" = Just "audio/mpeg"
contentTypeFromFileExtension "mp4" = Just "video/mp4"
contentTypeFromFileExtension "mpeg" = Just "video/mpeg"
contentTypeFromFileExtension "mpkg" = Just "application/vnd.apple.installer+xml"
contentTypeFromFileExtension "odp" = Just "application/vnd.oasis.opendocument.presentation"
contentTypeFromFileExtension "ods" = Just "application/vnd.oasis.opendocument.spreadsheet"
contentTypeFromFileExtension "odt" = Just "application/vnd.oasis.opendocument.text"
contentTypeFromFileExtension "oga" = Just "audio/ogg"
contentTypeFromFileExtension "ogv" = Just "video/ogg"
contentTypeFromFileExtension "ogx" = Just "application/ogg"
contentTypeFromFileExtension "opus" = Just "audio/opus"
contentTypeFromFileExtension "otf" = Just "font/otf"
contentTypeFromFileExtension "png" = Just "image/png"
contentTypeFromFileExtension "pdf" = Just "application/pdf"
contentTypeFromFileExtension "php" = Just "application/x-httpd-php"
contentTypeFromFileExtension "ppt" = Just "application/vnd.ms-powerpoint"
contentTypeFromFileExtension "pptx" = Just "application/vnd.openxmlformats-officedocument.presentationml.presentation"
contentTypeFromFileExtension "rar" = Just "application/vnd.rar"
contentTypeFromFileExtension "rtf" = Just "application/rtf"
contentTypeFromFileExtension "sh" = Just "application/x-sh"
contentTypeFromFileExtension "svg" = Just "image/svg+xml"
contentTypeFromFileExtension "swf" = Just "application/x-shockwave-flash"
contentTypeFromFileExtension "tar" = Just "application/x-tar"
contentTypeFromFileExtension "tif" = Just "image/tiff"
contentTypeFromFileExtension "tiff" = Just "image/tiff"
contentTypeFromFileExtension "ts" = Just "video/mp2t"
contentTypeFromFileExtension "ttf" = Just "font/ttf"
contentTypeFromFileExtension "txt" = Just "text/plain"
contentTypeFromFileExtension "vsd" = Just "application/vnd.visio"
contentTypeFromFileExtension "wav" = Just "audio/wav"
contentTypeFromFileExtension "weba" = Just "audio/webm"
contentTypeFromFileExtension "webm" = Just "video/webm"
contentTypeFromFileExtension "webp" = Just "image/webp"
contentTypeFromFileExtension "woff" = Just "font/woff"
contentTypeFromFileExtension "woff2" = Just "font/woff2"
contentTypeFromFileExtension "xhtml" = Just "application/xhtml+xml"
contentTypeFromFileExtension "xls" = Just "application/vnd.ms-excel"
contentTypeFromFileExtension "xlsx" = Just "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
contentTypeFromFileExtension "xml" = Just "application/xml"
contentTypeFromFileExtension "xul" = Just "application/vnd.mozilla.xul+xml"
contentTypeFromFileExtension "zip" = Just "application/zip"
contentTypeFromFileExtension "7z" = Just "application/x-7z-compressed"
contentTypeFromFileExtension _ = Nothing

runTCPServerM :: (MonadUnliftIO m) => Maybe String -> String -> (Socket -> m ()) -> m ()
runTCPServerM hostName port handler =
  withRunInIO $ \runInIO -> runTCPServer hostName port (runInIO . handler)
  
type MiddleWare m c1 c2 = Bidi (Kleisli m) (Request c1) (Request c2) (Response c2) (Response c1)

runServerRoutes ::
  (MonadUnliftIO m, MonadLogger m) =>
  Maybe String ->
  String ->
  HttpAuthority ->
  MiddleWare (RequestMonadT (MReader.ReaderT Socket m)) () c ->
  RouteHandler (RequestMonadT (MReader.ReaderT Socket m)) c ->
  m ()
runServerRoutes hostName port defaultAuthority middleware handler =
  runTCPServerM
    hostName
    port
    (MReader.runReaderT (handleServerRoutes defaultAuthority middleware handler))

handleServerRoutes ::
  (MonadUnliftIO m, MonadReader Socket m, MonadLogger m) =>
  HttpAuthority ->
  MiddleWare (RequestMonadT m) () c ->
  RouteHandler (RequestMonadT m) c ->
  m ()
handleServerRoutes defaultAuthority middleware handler =
  handleServer defaultAuthority (middleware .|| Kleisli (routeHandlerOrSimpleNotFound handler))

runServer ::
  (MonadUnliftIO m, MonadLogger m) =>
  Maybe String ->
  String ->
  HttpAuthority ->
  Kleisli (RequestMonadT (MReader.ReaderT Socket m)) (Request ()) (Response ()) ->
  m ()
runServer hostName port defaultAuthority handleRequest =
  runTCPServerM
    hostName
    port
    (MReader.runReaderT (handleServer defaultAuthority handleRequest))

handleServer ::
  (MonadUnliftIO m, MonadReader Socket m, MonadLogger m) =>
  HttpAuthority ->
  Kleisli (RequestMonadT m) (Request ()) (Response ()) ->
  m ()
handleServer defaultAuthority handleRequest =
  C.handleServer
    defaultAuthority
    ( Kleisli
        ( \(req, body) ->
            requestToConnectionMonad
              req
              (fmap connectionToRequestMonad body)
              (runKleisli (transformRequestResponse .| setCorrectnessHeaders .|| handleRequest) req)
        )
    )
