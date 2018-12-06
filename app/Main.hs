module Main where

import Lib
import Config
import Network.Wai.Handler.Warp (run)
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

main :: IO ()
main = do
  cfg <- readConfigFromEnv
  myKey <- generateKey
  run (configPort cfg) (app myKey cfg)
