{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module BareBonesHttp.Http.Internal.Connections
  ( handleServer,
    handleServerRaw,
    ConnectionState (..),
    bytesRemaining,
    closeWhenDone,
    bodyBytes,
    Header (..),
    headerName,
    headerContent,
    RequestInfo (..),
    requestMethod,
    requestUri,
    requestProtocol,
    requestHeaders,
    ResponseInfo (..),
    responseProtocol,
    responseStatus,
    responseHeaders,
    responseBody,
    responseToBytes,
    ConnectionMonad (..),
  )
where

import BareBonesHttp.Bidi
import BareBonesHttp.Http.Definitions hiding
  ( requestHeaders,
    requestMethod,
    requestUri,
    responseBody,
    responseHeaders,
    responseStatus,
  )
import qualified BareBonesHttp.Uri.Parsers as UriP
import Control.Applicative (liftA2)
import Control.Arrow
import Control.Lens
import Control.Monad (when)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as MExcept
import qualified Control.Monad.Trans.State as MState
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.CaseInsensitive as CI
import Data.Char (isHexDigit, isSpace)
import Data.Foldable (foldlM)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as T
import Network.Socket
import Network.Socket.ByteString
import Numeric
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import UnliftIO.Exception hiding (fromEither)

-------------------- Misc utils --------------------

whileM :: Monad m => (a -> Bool) -> m a -> m [a]
whileM cond ma = loopAcc []
  where
    loopAcc acc = do
      a <- ma
      if cond a
        then loopAcc $ a : acc
        else pure (reverse (a : acc))

fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a

isNewline :: B.ByteString -> Bool
isNewline c = c == "\r\n" || c == "\n"

flipMaybe :: a -> Maybe b -> Maybe a
flipMaybe _ (Just _) = Nothing
flipMaybe a Nothing = Just a

-------------------- Connection state --------------------

data ConnectionState = ConnectionState
  { _bytesRemaining :: B.ByteString,
    _closeWhenDone :: Bool,
    _bodyBytes :: Maybe B.ByteString
  }
  deriving (Eq, Show)

makeLenses ''ConnectionState

freshConnectionState :: B.ByteString -> ConnectionState
freshConnectionState leftoverBytes = ConnectionState leftoverBytes False Nothing

-------------------- Requests and responses --------------------

data Header = Header
  { _headerName :: CI.CI T.Text,
    _headerContent :: T.Text
  }
  deriving (Eq)

makeLenses ''Header

instance Show Header where
  show (Header name content) = T.unpack (CI.original name) ++ ": " ++ T.unpack content

data RequestInfo = RequestInfo
  { _requestMethod :: Method,
    _requestUri :: HttpUri,
    _requestProtocol :: ProtocolVersion,
    _requestHeaders :: [Header]
  }

makeLenses ''RequestInfo

instance Show RequestInfo where
  show (RequestInfo method uri protocol headers) =
    show method ++ " " ++ show uri ++ "HTTP/" ++ show protocol ++ "\r\n"
      ++ concatMap (\h -> show h ++ "\r\n") headers
      ++ "\r\n"

data ResponseInfo = ResponseInfo
  { _responseProtocol :: ProtocolVersion,
    _responseStatus :: Status,
    _responseHeaders :: [Header],
    _responseBody :: Maybe B.ByteString
  }

makeLenses ''ResponseInfo

instance Show ResponseInfo where
  show (ResponseInfo protocol status headers body) =
    "HTTP/" ++ show protocol ++ " " ++ show status
      ++ concatMap (\h -> show h ++ "\r\n") headers
      ++ "\r\n"
      ++ maybe "" (T.unpack . TE.decodeUtf8) body

responseToBytes :: (Arrow a) => a ResponseInfo [B.ByteString]
responseToBytes =
  arr
    ( \(ResponseInfo protocol status headers body) ->
        let responseLine = TE.encodeUtf8 ("HTTP/" <> T.pack (show protocol) <> " " <> T.pack (show status) <> "\r\n")
         in let headerLines = fmap (\h -> TE.encodeUtf8 $ CI.original (_headerName h) <> ": " <> _headerContent h <> "\r\n") headers ++ ["\r\n"]
             in maybe (responseLine : headerLines) (\b -> responseLine : (headerLines ++ [b])) body
    )

-------------------- Read data --------------------

readMoreN ::
  ( MonadIO m,
    MonadReader Socket m,
    MonadState ConnectionState m,
    MonadError ResponseInfo m,
    MonadLogger m
  ) =>
  Int ->
  m Int
readMoreN n = do
  $(logDebug) ("Trying to read " <> T.pack (show n) <> " more bytes")
  current <- use bytesRemaining
  s <- ask
  more <- liftIO $ recv s n
  bytesRemaining .= (current <> more)
  $(logDebug) ("Read " <> T.pack (show (B.length more)) <> " more bytes")
  when
    (B.null more)
    ( do
        $(logDebug) "Errored on request. Reason: Expected more bytes"
        throwError (makeErrorResponse BadRequest "Expected more bytes")
    )
  return $ B.length more

readN ::
  ( MonadIO m,
    MonadReader Socket m,
    MonadState ConnectionState m,
    MonadError ResponseInfo m,
    MonadLogger m
  ) =>
  Int ->
  m B.ByteString
readN n = do
  current <- use bytesRemaining
  let remaining = n - B.length current
  if remaining <= 0
    then let (ret, newState) = B.splitAt n current in ret <$ (bytesRemaining .= newState)
    else readMoreN 1024 *> readN n

readLine ::
  ( MonadIO m,
    MonadReader Socket m,
    MonadState ConnectionState m,
    MonadError ResponseInfo m,
    MonadLogger m
  ) =>
  m B.ByteString
readLine = do
  $(logDebug) "Reading until newline"
  current <- use bytesRemaining
  case splitNewline current of
    Just (ret, newState) -> do
      $(logDebug)
        ( "Found newline. Read: " <> TE.decodeUtf8 ret
            <> " Raw data: "
            <> T.pack (B.unpack ret >>= (`showHex` ""))
        )
      bytesRemaining .= newState
      return ret
    Nothing -> do
      $(logDebug) "Reading more bytes for newline"
      _ <- readMoreN 1024
      readLine
  where
    splitNewline :: B.ByteString -> Maybe (B.ByteString, B.ByteString)
    splitNewline b = splitNewlineAtIdx <$> BC.elemIndex '\n' b
      where
        splitNewlineAtIdx i = let (before, after) = BC.splitAt i b in (removeCr before, BC.drop 1 after)
        removeCr before = if not (B.null before) && BC.last before == '\r' then BC.init before else before

-------------------- Errors --------------------

makeErrorResponse :: Status -> T.Text -> ResponseInfo
makeErrorResponse s err =
  let bytesErr = TE.encodeUtf8 (err <> "\r\n")
   in let headers = [Header "Connection" "close", Header "Content-Length" (T.pack $ show $ B.length bytesErr)]
       in ResponseInfo Http11 s headers (Just bytesErr)

aError :: (MonadError ResponseInfo m, MonadLogger m) => Kleisli m (Status, T.Text) b
aError =
  Kleisli
    ( \(status, text) -> do
        $(logDebug) ("Errored on request. Reason: " <> text)
        throwError $ makeErrorResponse status text
    )

aSpecificErrorLeft :: (MonadError ResponseInfo m, MonadLogger m) => Kleisli m (Either (Status, T.Text) a) a
aSpecificErrorLeft = aError ||| arr id

aErrorLeft :: (MonadError ResponseInfo m, MonadLogger m) => Kleisli m (Either T.Text a) a
aErrorLeft = ((BadRequest,) ^>> aError) ||| arr id

aErrorNothing :: (MonadError ResponseInfo m, MonadLogger m) => T.Text -> Kleisli m (Maybe a) a
aErrorNothing errorMsg = maybeToRight errorMsg ^>> aErrorLeft
  where
    maybeToRight :: a -> Maybe b -> Either a b
    maybeToRight _ (Just b) = Right b
    maybeToRight a Nothing = Left a

aErrorIf :: (MonadError ResponseInfo m, MonadLogger m) => (a -> Bool) -> T.Text -> Kleisli m a a
aErrorIf f t = (\a -> if f a then Nothing else Just a) ^>> aErrorNothing t

-------------------- Receive and encode requests and responses --------------------

receiveRequestInfoBytes ::
  ( MonadIO m,
    MonadState ConnectionState m,
    MonadReader Socket m,
    MonadError ResponseInfo m,
    MonadLogger m
  ) =>
  m [B.ByteString]
receiveRequestInfoBytes = do
  requestLine <- last <$> whileM B.null readLine
  $(logDebug) ("Got request line: " <> TE.decodeUtf8 requestLine)
  headers <- init <$> whileM (not . B.null) readLine
  $(logDebug) ("Read headers:\n" <> TE.decodeUtf8 (BC.unlines headers))

  --We'll complain about no headers later
  let processedHeaders = dropWhile (isSpace . BC.head) headers
  return (requestLine : processedHeaders)

parseRequestLine ::
  ( MonadState ConnectionState m,
    MonadError ResponseInfo m,
    MonadLogger m
  ) =>
  Kleisli m B.ByteString (Method, HttpDestination, ProtocolVersion)
parseRequestLine = BC.split ' ' ^>> exactly3 "Invalid request line" >>> parseRequestLineParts >>> flatten3
  where
    exactly3 :: (MonadError ResponseInfo m, MonadLogger m) => T.Text -> Kleisli m [a] (a, (a, a))
    exactly3 errorMsg =
      ( \case
          [a, b, c] -> Right (a, (b, c))
          _ -> Left errorMsg
      )
        ^>> aErrorLeft

    flatten3 = arr (\(method, (uri, protocol)) -> (method, uri, protocol))

    parseMethod :: (MonadError ResponseInfo m, MonadLogger m) => Kleisli m B.ByteString Method
    parseMethod = TE.decodeUtf8 ^>> readMethod ^>> aErrorNothing "Invalid method"

    --TODO: Handle other destination forms too
    parseUriDestination :: (MonadError ResponseInfo m, MonadLogger m) => Kleisli m B.ByteString HttpDestination
    parseUriDestination = TE.decodeUtf8 ^>> readHttpDestination ^>> aErrorNothing "Invalid URI destination"

    parseProtocol :: (MonadError ResponseInfo m, MonadLogger m) => Kleisli m B.ByteString ProtocolVersion
    parseProtocol = TE.decodeUtf8 ^>> readProtocolVersion ^>> aErrorNothing "Invalid protocol version"

    parseRequestLineParts = parseMethod *** parseUriDestination *** parseProtocol

parseHeaders :: (MonadError ResponseInfo m, MonadLogger m) => Kleisli m [B.ByteString] [Header]
parseHeaders =
  ( over (_Just . traverse) parseHeader
      . foldMap
        ( \(x, xs) ->
            NE.toList
              <$> (separateHeaderLineWithTail [] x >>= (\t -> foldlM (flip separateHeaderLines) t xs))
        )
      . uncons
  )
    ^>> aErrorNothing "Invalid headers"
  where
    --Yes, really Latin1, but only for historic reasons
    parseHeader :: (B.ByteString, B.ByteString) -> Header
    parseHeader = uncurry Header . over _1 CI.mk . over both TE.decodeLatin1
    separateHeaderLine :: B.ByteString -> Maybe (B.ByteString, B.ByteString)
    separateHeaderLine x = case BC.break (== ':') x of
      (_, "") -> Nothing
      (hName, hValue) -> flipMaybe (hName, BC.strip hValue) (BC.findIndex isSpace hName)

    separateHeaderLineWithTail ::
      [(B.ByteString, B.ByteString)] ->
      B.ByteString ->
      Maybe (NE.NonEmpty (B.ByteString, B.ByteString))
    separateHeaderLineWithTail t x = (:| t) <$> separateHeaderLine x

    separateHeaderLines ::
      B.ByteString ->
      NE.NonEmpty (B.ByteString, B.ByteString) ->
      Maybe (NE.NonEmpty (B.ByteString, B.ByteString))
    separateHeaderLines x (lastH@(lastHName, lastHValue) :| acc)
      | BC.head x == ' ' = Just ((lastHName, lastHValue <> BC.strip x) :| acc)
      | otherwise = separateHeaderLineWithTail (lastH : acc) x

parseRequestInfo ::
  ( MonadState ConnectionState m,
    MonadError ResponseInfo m,
    MonadLogger m
  ) =>
  HttpAuthority ->
  Kleisli m [B.ByteString] RequestInfo
parseRequestInfo defaultAuthority =
  requireOne "request line" >>> (parseRequestLine *** parseHeaders) >>> makeRequestInfo
  where
    requireOne :: (MonadError ResponseInfo m, MonadLogger m) => T.Text -> Kleisli m [a] (a, [a])
    requireOne unit = uncons ^>> aErrorNothing ("At least one " <> unit <> "required")

    makeUri headers (HttpDestination path query) protocol =
      let authority = case (find (\h -> _headerName h == "Host") headers, protocol) of
            (Just (Header _ host), _) ->
              let hostParser = liftA2 HttpAuthority UriP.host (P.optional (P.string ":" *> UriP.port))
               in case P.runParser hostParser "" host of
                    Right r -> Right r
                    Left e -> Left (T.pack (P.errorBundlePretty e))
            (Nothing, Http10) -> Right defaultAuthority
            (Nothing, _) -> Left "Host header required"
       in fmap (\a -> HttpUri a path query) authority

    makeRequestInfo ::
      (MonadError ResponseInfo m, MonadLogger m) =>
      Kleisli m ((Method, HttpDestination, ProtocolVersion), [Header]) RequestInfo
    makeRequestInfo =
      ( \((method, destination, protocol), headers) ->
          fmap (\uri' -> RequestInfo method uri' protocol headers) (makeUri headers destination protocol)
      )
        ^>> aErrorLeft

receiveRequestBodyNormalBytes ::
  ( MonadIO m,
    MonadState ConnectionState m,
    MonadReader Socket m,
    MonadError ResponseInfo m,
    MonadLogger m
  ) =>
  Int ->
  m (B.ByteString, [Header])
receiveRequestBodyNormalBytes n = (,[]) <$> readN n

receiveRequestBodyChunked ::
  ( MonadIO m,
    MonadState ConnectionState m,
    MonadReader Socket m,
    MonadError ResponseInfo m,
    MonadLogger m
  ) =>
  m (B.ByteString, [Header])
receiveRequestBodyChunked = do
  --Haha funny face :P
  chunks <- ((<$>) . (<$>)) snd (whileM (not . fst) receiveChunk)
  trailerParts <- init <$> whileM (not . isNewline) readLine
  headers <- runKleisli parseHeaders trailerParts

  -- TODO: Somehow pass in chunk extensions
  pure (B.concat (fmap fst chunks), headers)
  where
    receiveChunk ::
      ( MonadIO m,
        MonadState ConnectionState m,
        MonadReader Socket m,
        MonadError ResponseInfo m,
        MonadLogger m
      ) =>
      m (Bool, (B.ByteString, [(B.ByteString, Maybe B.ByteString)]))
    receiveChunk = do
      (size, chunkExtensions) <- readLine >>= runKleisli (BC.break (not . isHexDigit) ^>> parseSize *** parseChunkExtensions)
      chunkData <-
        if size == 0
          then pure B.empty
          else readN size <* ensureNewline
      pure (size == 0, (chunkData, chunkExtensions))

    parseSize :: (MonadError ResponseInfo m, MonadLogger m) => Kleisli m B.ByteString Int
    parseSize = hexToInt ^>> aErrorNothing ""

    ensureNewline ::
      ( MonadIO m,
        MonadState ConnectionState m,
        MonadReader Socket m,
        MonadError ResponseInfo m,
        MonadLogger m
      ) =>
      m ()
    ensureNewline = do
      successfulEnd <- B.null <$> readLine
      if successfulEnd
        then pure ()
        else runKleisli aError (BadRequest, "More data on chunk than expected")

    hexToInt hex = case readHex (T.unpack $ TE.decodeUtf8 hex) of
      [(n, "")] -> Just n
      _ -> Nothing

    --TODO: Use a proper parser instead of this?
    parseChunkExtensions :: (MonadError ResponseInfo m, MonadLogger m) => Kleisli m B.ByteString [(B.ByteString, Maybe B.ByteString)]
    parseChunkExtensions =
      BC.break (== ';')
        ^>> first (requireLength (== 0))
        >>> snd
        ^>> leftOn ((0 ==) . B.length)
        >>> ( arr (const [])
                +++ ( requireLength (> 1)
                        >>> BC.break (== ';')
                        ^>> ( ( BC.break (== '=')
                                  ^>> second (requireLength (\l -> l == 0 || l > 1))
                                  >>^ second (\b -> if B.null b then Nothing else Just b)
                              )
                                *** parseChunkExtensions
                            )
                        >>^ uncurry (:)
                    )
            )
        >>^ fromEither
      where
        leftOn :: (Arrow a) => (b -> Bool) -> a b (Either b b)
        leftOn f = arr (\a -> if f a then Left a else Right a)

        requireLength :: (MonadError ResponseInfo m, MonadLogger m) => (Int -> Bool) -> Kleisli m B.ByteString B.ByteString
        requireLength f = aErrorIf (f . B.length) "Chunk extension length not satisfied"

receiveRequestBody ::
  ( MonadIO m,
    MonadState ConnectionState m,
    MonadReader Socket m,
    MonadError ResponseInfo m,
    MonadLogger m
  ) =>
  Kleisli m RequestInfo (RequestInfo, Maybe (m (B.ByteString, [Header])))
receiveRequestBody =
  ( \requestInfo ->
      --TODO: Handle multiple headers matching
      case ( findHeader requestInfo "Content-Length" >>= (textToNum . _headerContent),
             findHeader requestInfo "Transfer-Coding" <&> (fmap T.toLower . T.splitOn ", " . _headerContent)
           ) of
        (Nothing, Nothing) -> Right (requestInfo, Nothing)
        (Just 0, Nothing) -> Right (requestInfo, Nothing)
        (Just contentLength, Nothing) -> Right (requestInfo, Just $ receiveRequestBodyNormalBytes contentLength)
        (Nothing, Just ["chunked"]) -> Right (requestInfo, Just receiveRequestBodyChunked)
        -- TODO: Respond with 400 if chunked is in the list, but not the last
        (Nothing, Just _) -> Left (NotImplemented, "Only chunked transfer coding supported")
        -- From what I understand messages like these can be allowed,
        -- as long as the content length is ignored, but this is simpler
        (Just _, Just _) -> Left (BadRequest, "Received both Content-Length and Transfer-Coding")
  )
    ^>> aSpecificErrorLeft
  where
    findHeader ri name = findOf (requestHeaders . traverse) (\h -> _headerName h == name) ri
    textToNum :: T.Text -> Maybe Int
    textToNum t = case T.decimal t of
      Right (n, "") -> Just n
      _ -> Nothing

-------------------- Server functions --------------------

newtype ConnectionMonad m a = ConnectionMonad
  {runConnection :: MExcept.ExceptT ResponseInfo (MState.StateT ConnectionState m) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState ConnectionState,
      MonadError ResponseInfo,
      MonadLogger
    )

instance MonadReader r m => MonadReader r (ConnectionMonad m) where
  ask = ConnectionMonad (lift ask)
  local f (ConnectionMonad m) = ConnectionMonad (local f m)

byteConvertLayer ::
  ( MonadIO m,
    MonadState ConnectionState m,
    MonadError ResponseInfo m,
    MonadReader Socket m,
    MonadLogger m
  ) =>
  HttpAuthority ->
  Bidi (Kleisli m) [B.ByteString] (RequestInfo, Maybe (m (B.ByteString, [Header]))) ResponseInfo [B.ByteString]
byteConvertLayer defaultAuthority = Bidi (parseRequestInfo defaultAuthority >>> receiveRequestBody) responseToBytes

handleServer ::
  ( MonadUnliftIO m,
    MonadReader Socket m,
    MonadLogger m
  ) =>
  HttpAuthority ->
  Kleisli (ConnectionMonad m) (RequestInfo, Maybe (ConnectionMonad m (B.ByteString, [Header]))) ResponseInfo ->
  m ()
handleServer defaultAuthority handleRequest =
  handleServerRaw $ byteConvertLayer defaultAuthority .|| handleRequest

handleServerRaw ::
  ( MonadUnliftIO m,
    MonadReader Socket m,
    MonadLogger m
  ) =>
  Kleisli (ConnectionMonad m) [B.ByteString] [B.ByteString] ->
  m ()
handleServerRaw handleRequest = loopRun ""
  where
    loopRun input = do
      $(logInfo) "Waiting for new request"
      s <- ask
      ret <- tryAny $
        resolveConnectionEffects input $ do
          requestInfoBytes <- receiveRequestInfoBytes
          $(logInfo) ("Received request:\n" <> TE.decodeUtf8 (BC.unlines requestInfoBytes))
          runKleisli handleRequest requestInfoBytes

      case ret of
        Right (Right response, connectionState) -> do
          liftIO $ sendMany s response
          if connectionState ^. closeWhenDone
            then pure ()
            else loopRun (connectionState ^. bytesRemaining)
        Right (Left err, _) -> do
          liftIO $ sendMany s (responseToBytes err)
          pure ()
        Left ex ->
          if isAsyncException ex
            then throwIO ex
            else do
              liftIO $
                sendMany
                  s
                  (responseToBytes (makeErrorResponse InternalServerError (T.pack (displayException ex))))
              pure ()

    resolveConnectionEffects :: B.ByteString -> ConnectionMonad m [B.ByteString] -> m (Either ResponseInfo [B.ByteString], ConnectionState)
    resolveConnectionEffects byteInput (ConnectionMonad (MExcept.ExceptT m)) =
      MState.runStateT m (freshConnectionState byteInput)
