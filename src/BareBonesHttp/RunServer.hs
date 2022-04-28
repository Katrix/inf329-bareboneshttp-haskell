module BareBonesHttp.RunServer where

import Network.Run.TCP

foo :: IO ()
foo = do
  let foo' = runTCPServer Nothing "9000"
  return ()
