{-# LANGUAGE OverloadedStrings #-}

module BareBonesHttp.Http.Definitions.HttpStatus where

import qualified Data.Text as T

data Status
  = --Informational
    Continue
  | SwitchingProtocols
  | --Successful
    Ok
  | Created
  | Accepted
  | NonAuthoritativeInformation
  | NoContent
  | ResetContent
  | PartialContent
  | --Redirection
    MultipleChoice
  | MovedPermanently
  | Found
  | SeeOther
  | NotModified
  | UseProxy
  | TemporaryRedirect
  | --ClientError
    BadRequest
  | Unauthorized
  | PaymentRequired
  | Forbidden
  | NotFound
  | MethodNotAllowed
  | NotAcceptable
  | ProxyAuthenticationRequired
  | RequestTimeout
  | Conflict
  | Gone
  | LengthRequired
  | PreconditionFailed
  | PayloadTooLarge
  | UriTooLong
  | UnsupportedMediaType
  | RangeNotSatisfiable
  | ExpectationFailed
  | UpgradeRequired
  | --ServerError
    InternalServerError
  | NotImplemented
  | BadGateway
  | ServiceUnavailable
  | GatewayTimeout
  | HttpVersionNotSupported
  | Other Int T.Text

instance Eq Status where
  a == b = statusCode a == statusCode b

instance Show Status where
  show s = show (statusCode s) ++ " " ++ T.unpack (statusMessage s)

statusCode :: Status -> Int
--Informational
statusCode Continue = 100
statusCode SwitchingProtocols = 101
--Successful
statusCode Ok = 200
statusCode Created = 201
statusCode Accepted = 202
statusCode NonAuthoritativeInformation = 203
statusCode NoContent = 204
statusCode ResetContent = 205
statusCode PartialContent = 206
--Redirection
statusCode MultipleChoice = 300
statusCode MovedPermanently = 301
statusCode Found = 302
statusCode SeeOther = 303
statusCode NotModified = 304
statusCode UseProxy = 305
statusCode TemporaryRedirect = 307
--ClientError
statusCode BadRequest = 400
statusCode Unauthorized = 401
statusCode PaymentRequired = 402
statusCode Forbidden = 403
statusCode NotFound = 404
statusCode MethodNotAllowed = 405
statusCode NotAcceptable = 406
statusCode ProxyAuthenticationRequired = 407
statusCode RequestTimeout = 408
statusCode Conflict = 409
statusCode Gone = 410
statusCode LengthRequired = 411
statusCode PreconditionFailed = 412
statusCode PayloadTooLarge = 413
statusCode UriTooLong = 414
statusCode UnsupportedMediaType = 415
statusCode RangeNotSatisfiable = 416
statusCode ExpectationFailed = 417
statusCode UpgradeRequired = 426
--ServerError
statusCode InternalServerError = 500
statusCode NotImplemented = 501
statusCode BadGateway = 502
statusCode ServiceUnavailable = 503
statusCode GatewayTimeout = 504
statusCode HttpVersionNotSupported = 505
--
statusCode (Other code _) = code

statusMessage :: Status -> T.Text
--Informational
statusMessage Continue = "Continue"
statusMessage SwitchingProtocols = "Switching Protocols"
--Successful
statusMessage Ok = "OK"
statusMessage Created = "Created"
statusMessage Accepted = "Accepted"
statusMessage NonAuthoritativeInformation = "Non-Authoritative Information"
statusMessage NoContent = "No Content"
statusMessage ResetContent = "Reset Content"
statusMessage PartialContent = "Partial Content"
--Redirection
statusMessage MultipleChoice = "Multiple Choice"
statusMessage MovedPermanently = "Moved Permantently"
statusMessage Found = "Found"
statusMessage SeeOther = "See Other"
statusMessage NotModified = "Not Modified"
statusMessage UseProxy = "Use Proxy"
statusMessage TemporaryRedirect = "Temporary Redirect"
--ClientError
statusMessage BadRequest = "Bad Request"
statusMessage Unauthorized = "Unauthorized"
statusMessage PaymentRequired = "Payment Required"
statusMessage Forbidden = "Forbidden"
statusMessage NotFound = "Not Found"
statusMessage MethodNotAllowed = "Method Not Allowed"
statusMessage NotAcceptable = "Not Acceptable"
statusMessage ProxyAuthenticationRequired = "Proxy Authentication Required"
statusMessage RequestTimeout = "Request Timeout"
statusMessage Conflict = "Conflict"
statusMessage Gone = "Gone"
statusMessage LengthRequired = "Length Required"
statusMessage PreconditionFailed = "Precondition Failed"
statusMessage PayloadTooLarge = "Payload Too Large"
statusMessage UriTooLong = "URI Too Long"
statusMessage UnsupportedMediaType = "Unsupported Media Type"
statusMessage RangeNotSatisfiable = "Range Not Satisfiable"
statusMessage ExpectationFailed = "Expectation Failed"
statusMessage UpgradeRequired = "Upgrade Required"
--ServerError
statusMessage InternalServerError = "Internal Server Error"
statusMessage NotImplemented = "Not Implemented"
statusMessage BadGateway = "Bad Gateway"
statusMessage ServiceUnavailable = "Service Unavailable"
statusMessage GatewayTimeout = "Gatewayb Timeout"
statusMessage HttpVersionNotSupported = "HTTP Version Not Supported"
--
statusMessage (Other _ message) = message

statusCodeOf :: Int -> T.Text -> Status
--Informational
statusCodeOf 100 _ = Continue
statusCodeOf 101 _ = SwitchingProtocols
--Successful
statusCodeOf 200 _ = Ok
statusCodeOf 201 _ = Created
statusCodeOf 202 _ = Accepted
statusCodeOf 203 _ = NonAuthoritativeInformation
statusCodeOf 204 _ = NoContent
statusCodeOf 205 _ = ResetContent
statusCodeOf 206 _ = PartialContent
--Redirection
statusCodeOf 300 _ = MultipleChoice
statusCodeOf 301 _ = MovedPermanently
statusCodeOf 302 _ = Found
statusCodeOf 303 _ = SeeOther
statusCodeOf 304 _ = NotModified
statusCodeOf 305 _ = UseProxy
statusCodeOf 307 _ = TemporaryRedirect
--ClientError
statusCodeOf 400 _ = BadRequest
statusCodeOf 401 _ = Unauthorized
statusCodeOf 402 _ = PaymentRequired
statusCodeOf 403 _ = Forbidden
statusCodeOf 404 _ = NotFound
statusCodeOf 405 _ = MethodNotAllowed
statusCodeOf 406 _ = NotAcceptable
statusCodeOf 407 _ = ProxyAuthenticationRequired
statusCodeOf 408 _ = RequestTimeout
statusCodeOf 409 _ = Conflict
statusCodeOf 410 _ = Gone
statusCodeOf 411 _ = LengthRequired
statusCodeOf 412 _ = PreconditionFailed
statusCodeOf 413 _ = PayloadTooLarge
statusCodeOf 414 _ = UriTooLong
statusCodeOf 415 _ = UnsupportedMediaType
statusCodeOf 416 _ = RangeNotSatisfiable
statusCodeOf 417 _ = ExpectationFailed
statusCodeOf 426 _ = UpgradeRequired
--ServerError
statusCodeOf 500 _ = InternalServerError
statusCodeOf 501 _ = NotImplemented
statusCodeOf 502 _ = BadGateway
statusCodeOf 503 _ = ServiceUnavailable
statusCodeOf 504 _ = GatewayTimeout
statusCodeOf 505 _ = HttpVersionNotSupported
--
statusCodeOf code message = Other code message

mkStatus :: Int -> T.Text -> Status
mkStatus code msg =
  let ourStatus = statusCodeOf code msg
   in if msg == statusMessage ourStatus then ourStatus else Other code msg

data StatusVariant
  = StatusInformational
  | StatusSuccessful
  | StatusRedirection
  | StatusClientError
  | StatusServerError
  | StatusVariantUnknown

statusVariant :: Status -> StatusVariant
statusVariant s = case statusCode s of
  x | x >= 100 && x < 200 -> StatusInformational
  x | x >= 200 && x < 300 -> StatusSuccessful
  x | x >= 300 && x < 400 -> StatusRedirection
  x | x >= 400 && x < 500 -> StatusClientError
  x | x >= 500 && x < 600 -> StatusServerError
  _ -> StatusVariantUnknown
