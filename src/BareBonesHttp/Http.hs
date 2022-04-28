{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module BareBonesHttp.Http
  ( module BareBonesHttp.Http.Definitions,
    module BareBonesHttp.Http.AsResponse,
    module BareBonesHttp.Http.Capabilities,
    module BareBonesHttp.Http.RouteHandler,
    RequestMonad (..),
    RequestMonadT (..),
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
import Control.Monad (unless)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader.Class
import qualified Control.Monad.Trans.Except as MExcept
import qualified Control.Monad.Trans.RWS as MRWS
import qualified Control.Monad.Trans.Reader as MReader
import qualified Control.Monad.Trans.State as MState
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Network.Run.TCP
import Network.Socket

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

transformRequestResponse :: (Arrow a) => Bidi a C.RequestInfo (Request ()) (Response c) C.ResponseInfo
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
  deriving (Functor, Applicative, Monad, MonadIO, MonadError (Response ()))

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

setCorrectnessHeaders :: (RequestMonad m) => Bidi (Kleisli m) (Request c1) (Request c1) (Response c2) (Response c2)
setCorrectnessHeaders = Bidi (arr id) (Kleisli ensureCloseCorrect >>> Kleisli ensureBodyRead >>^ setBodyContentLength)

-------------------- Server functions --------------------

runTCPServerM :: (MonadUnliftIO m) => Maybe String -> String -> (Socket -> m ()) -> m ()
runTCPServerM hostName port handler =
  withRunInIO $ \runInIO -> runTCPServer hostName port (runInIO . handler)

runServerRoutes ::
  (MonadUnliftIO m) =>
  Maybe String ->
  String ->
  HttpAuthority ->
  Bidi (Kleisli (RequestMonadT (MReader.ReaderT Socket m))) (Request ()) (Request c1) (Response c1) (Response c2) ->
  RouteHandler (RequestMonadT (MReader.ReaderT Socket m)) ->
  m ()
runServerRoutes hostName port defaultAuthority middleware handler =
  runTCPServerM
    hostName
    port
    (MReader.runReaderT (handleServerRoutes defaultAuthority middleware handler))

handleServerRoutes ::
  (MonadIO m, MonadReader Socket m) =>
  HttpAuthority ->
  Bidi (Kleisli (RequestMonadT m)) (Request ()) (Request c1) (Response c1) (Response c2) ->
  RouteHandler (RequestMonadT m) ->
  m ()
handleServerRoutes defaultAuthority middleware handler =
  handleServer defaultAuthority (middleware .|| Kleisli (routeHandlerOrSimpleNotFound handler))

runServer ::
  (MonadUnliftIO m) =>
  Maybe String ->
  String ->
  HttpAuthority ->
  Kleisli (RequestMonadT (MReader.ReaderT Socket m)) (Request ()) (Response c) ->
  m ()
runServer hostName port defaultAuthority handleRequest =
  runTCPServerM
    hostName
    port
    (MReader.runReaderT (handleServer defaultAuthority handleRequest))

handleServer ::
  (MonadIO m, MonadReader Socket m) =>
  HttpAuthority ->
  Kleisli (RequestMonadT m) (Request ()) (Response c) ->
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
