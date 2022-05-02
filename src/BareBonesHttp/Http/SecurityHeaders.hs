{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module BareBonesHttp.Http.SecurityHeaders where

import BareBonesHttp.Bidi
import BareBonesHttp.Http
import Control.Arrow
import Control.Lens
import Control.Monad.IO.Class
import qualified Crypto.Nonce as Nonce
import qualified Data.Map as Map
import qualified Data.Text as T

addSecurityHeaders :: (Arrow a) => Bidi a (Request ic) (Request ic) (Response oc) (Response oc)
addSecurityHeaders = Bidi (arr id) (arr addHeaders)
  where
    addHeaders resp = over responseHeaders (\h -> h `Map.union` Map.fromList headers) resp
    headers =
      [ ("X-Frame-Options", "deny"),
        ("X-Content-Type-Options", "nosniff"),
        ("Referrer-Policy", "origin-when-cross-origin"),
        ("X-Permitted-Cross-Domain-Policies", "master-only"),
        ("X-XSS-Protection", "0")
      ]

newtype CspNonce = CspNonce {cspNonce :: T.Text}

addContentSecurityPolicy ::
  (MonadIO m) =>
  Nonce.Generator ->
  (T.Text -> T.Text) ->
  MiddleWare m c (AddCap CspNonce c)
addContentSecurityPolicy generator policy = Bidi (Kleisli addNonce) (arr addHeaders)
  where
    addNonce :: (MonadIO m) => Request c -> m (Request (AddCap CspNonce c))
    addNonce req = (\n -> over requestCap (addCap (CspNonce n)) req) <$> Nonce.nonce128urlT generator
    addHeaders resp =
      ( over
          responseCap
          removeCap
          . set
            (responseHeaders . at "Content-Security-Policy")
            ((Just . policy . cspNonce . getCap . (^. responseCap)) resp)
      )
        resp
