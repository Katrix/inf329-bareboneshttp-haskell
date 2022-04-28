{-# LANGUAGE OverloadedStrings #-}

module BareBonesHttp.Http.AsResponse where

import BareBonesHttp.Http.Definitions
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Text as T

--TODO: Improve and constrain the types here
class AsResponse a where
  encodeBody :: a -> B.ByteString

  contentType :: a -> T.Text

makeResponse :: (AsResponse a) => Status -> Request c -> a -> Response c
makeResponse s r a = Response s (Map.singleton "Content-Type" (contentType a)) Map.empty (Just $ encodeBody a) (_requestCap r)

--Successful
ok :: AsResponse a => Request c -> a -> Response c
ok = makeResponse Ok

created :: AsResponse a => Request c -> a -> Response c
created = makeResponse Created

accepted :: AsResponse a => Request c -> a -> Response c
accepted = makeResponse Accepted

nonAuthoritativeInformation :: AsResponse a => Request c -> a -> Response c
nonAuthoritativeInformation = makeResponse NonAuthoritativeInformation

noContent :: Request c -> Response c
noContent r = Response NoContent Map.empty Map.empty Nothing (_requestCap r)

resetContent :: AsResponse a => Request c -> a -> Response c
resetContent = makeResponse ResetContent

partialContent :: AsResponse a => Request c -> a -> Response c
partialContent = makeResponse PartialContent

--Redirection
multipleChoice :: AsResponse a => Request c -> a -> Response c
multipleChoice = makeResponse MultipleChoice

movedPermanently :: AsResponse a => Request c -> a -> Response c
movedPermanently = makeResponse MovedPermanently

found :: AsResponse a => Request c -> a -> Response c
found = makeResponse Found

seeOther :: AsResponse a => Request c -> a -> Response c
seeOther = makeResponse SeeOther

notModified :: AsResponse a => Request c -> a -> Response c
notModified = makeResponse NotModified

temporaryRedirect :: AsResponse a => Request c -> a -> Response c
temporaryRedirect = makeResponse TemporaryRedirect

--ClientError
badRequest :: AsResponse a => Request c -> a -> Response c
badRequest = makeResponse BadRequest

unauthorized :: AsResponse a => Request c -> a -> Response c
unauthorized = makeResponse Unauthorized

paymentRequired :: AsResponse a => Request c -> a -> Response c
paymentRequired = makeResponse PaymentRequired

forbidden :: AsResponse a => Request c -> a -> Response c
forbidden = makeResponse Forbidden

notFound :: AsResponse a => Request c -> a -> Response c
notFound = makeResponse NotFound

methodNotAllowed :: AsResponse a => Request c -> a -> Response c
methodNotAllowed = makeResponse MethodNotAllowed

notAcceptable :: AsResponse a => Request c -> a -> Response c
notAcceptable = makeResponse NotAcceptable

proxyAuthenticationRequired :: AsResponse a => Request c -> a -> Response c
proxyAuthenticationRequired = makeResponse ProxyAuthenticationRequired

requestTimeout :: AsResponse a => Request c -> a -> Response c
requestTimeout = makeResponse RequestTimeout

conflict :: AsResponse a => Request c -> a -> Response c
conflict = makeResponse Conflict

gone :: AsResponse a => Request c -> a -> Response c
gone = makeResponse Gone

lengthRequired :: AsResponse a => Request c -> a -> Response c
lengthRequired = makeResponse LengthRequired

preconditionFailed :: AsResponse a => Request c -> a -> Response c
preconditionFailed = makeResponse PreconditionFailed

payloadTooLarge :: AsResponse a => Request c -> a -> Response c
payloadTooLarge = makeResponse PayloadTooLarge

uriTooLong :: AsResponse a => Request c -> a -> Response c
uriTooLong = makeResponse UriTooLong

unsupportedMediaType :: AsResponse a => Request c -> a -> Response c
unsupportedMediaType = makeResponse UnsupportedMediaType

rangeNotSatisfiable :: AsResponse a => Request c -> a -> Response c
rangeNotSatisfiable = makeResponse RangeNotSatisfiable

expectationFailed :: AsResponse a => Request c -> a -> Response c
expectationFailed = makeResponse ExpectationFailed

--ServerError
internalServerError :: AsResponse a => Request c -> a -> Response c
internalServerError = makeResponse InternalServerError

notImplemented :: AsResponse a => Request c -> a -> Response c
notImplemented = makeResponse NotImplemented
