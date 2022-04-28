{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module BareBonesHttp.Http.LogTimeSpent where

import BareBonesHttp.Bidi
import BareBonesHttp.Http.Capabilities
import BareBonesHttp.Http.Definitions
import Control.Arrow
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Time

newtype StartedProcessing = StartedProcessing {startedProcessingTime :: UTCTime}

addContentSecurityPolicy ::
  (MonadIO m, AddCap StartedProcessing ic1 ic2, HasCap StartedProcessing oc) =>
  Bidi (Kleisli m) (Request ic1) (Request ic2) (Response oc) (Response oc)
addContentSecurityPolicy = Bidi (Kleisli setStartedTime) (Kleisli addHeaders)
  where
    setStartedTime :: (MonadIO m, AddCap StartedProcessing ic1 ic2) => Request ic1 -> m (Request ic2)
    setStartedTime req = (\t -> over requestCap (addCap (StartedProcessing t)) req) <$> liftIO getCurrentTime
    addHeaders resp =
      ( \newT ->
          set
            (responseHeaders . at "Route-Time")
            --God I hope this is correct. Appareantly only seconds exist in the Haskell world
            ( ( Just
                  . T.pack
                  . (show :: Integer -> String)
                  . round
                  . (/ 1000)
                  . nominalDiffTimeToSeconds
                  . diffUTCTime newT
                  . startedProcessingTime
                  . getCap
                  . (^. responseCap)
              )
                resp
            )
            resp
      )
        <$> liftIO getCurrentTime
