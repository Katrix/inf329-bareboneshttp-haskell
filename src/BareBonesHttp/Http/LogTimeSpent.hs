{-# LANGUAGE AllowAmbiguousTypes #-}
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

logTimeSpent ::
  (MonadIO m) =>
  Bidi (Kleisli m) (Request c) (Request (AddCap StartedProcessing c)) (Response (AddCap StartedProcessing c)) (Response c)
logTimeSpent = Bidi (Kleisli setStartedTime) (Kleisli addHeaders)
  where
    setStartedTime :: (MonadIO m) => Request c -> m (Request (AddCap StartedProcessing c))
    setStartedTime req = (\t -> over requestCap (addCap (StartedProcessing t)) req) <$> liftIO getCurrentTime
    addHeaders resp =
      ( \newT ->
          ( over
              responseCap
              removeCap
              . set
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
          )
            resp
      )
        <$> liftIO getCurrentTime
