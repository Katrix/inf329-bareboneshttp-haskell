{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module BareBonesHttp.Http.Definitions.CoreDefinitions where

import BareBonesHttp.Http.Definitions.HttpStatus
import BareBonesHttp.Http.Definitions.HttpUri
import Control.Lens
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import qualified Data.Text as T

data Method = Get | Head | Post | Put | Delete | Connect | Options | Trace | Patch deriving (Eq)

instance Show Method where
  show Get = "GET"
  show Head = "HEAD"
  show Post = "POST"
  show Put = "PUT"
  show Delete = "DELETE"
  show Connect = "CONNECT"
  show Options = "OPTIONS"
  show Trace = "TRACE"
  show Patch = "PATCH"

readMethod :: T.Text -> Maybe Method
readMethod "GET" = Just Get
readMethod "HEAD" = Just Head
readMethod "POST" = Just Post
readMethod "PUT" = Just Put
readMethod "DELETE" = Just Delete
readMethod "CONNECT" = Just Connect
readMethod "OPTIONS" = Just Options
readMethod "TRACE" = Just Trace
readMethod "PATCH" = Just Patch
readMethod _ = Nothing

data ProtocolVersion = Http10 | Http11 | UnknownHttpProtocolVersion T.Text deriving (Eq)

instance Show ProtocolVersion where
  show Http10 = "1.0"
  show Http11 = "1.1"
  show (UnknownHttpProtocolVersion version) = T.unpack version

readProtocolVersion :: T.Text -> Maybe ProtocolVersion
readProtocolVersion "HTTP/1.0" = Just Http10
readProtocolVersion "HTTP/1.1" = Just Http10
readProtocolVersion s =
  if T.isPrefixOf "HTTP/" s
    then Just $ UnknownHttpProtocolVersion (T.drop (T.length "HTTP/") s)
    else Nothing

type Headers = Map.Map (CI.CI T.Text) T.Text

data Request c = Request
  { _requestMethod :: Method,
    _requestUri :: HttpUri,
    _requestHeaders :: Headers,
    _requestCookies :: Map.Map T.Text T.Text,
    _requestCap :: c
  }
  deriving (Show {- , Eq (TODO: Eq for HttpUri) -}, Functor)

makeLenses ''Request

data Response c = Response
  { _responseStatus :: Status,
    _responseHeaders :: Headers,
    _responseCookies :: Map.Map T.Text T.Text,
    _responseBody :: Maybe B.ByteString,
    _responseCap :: c
  }
  deriving (Show, Eq)

makeLenses ''Response
