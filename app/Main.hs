{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Lib
import Config
import Network.Wai.Handler.Warp (run)
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Data.Time
import System.Environment
import System.FilePath
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import System.IO.Error

import ProjectM36.Client

main :: IO ()
main = do
  env <- getEnvironment
  let dataDir = fromMaybe "data" $ lookup "DBMS_CHECKER_DATA_DIR" env
      connInfo = InProcessConnectionInfo (CrashSafePersistence $ dataDir </> "database") emptyNotificationCallback []
  conn <- either (error . show) id <$> connectProjectM36 connInfo
  let cfg = Config {
    configPort       = maybe 8081 read $ lookup "DBMS_CHECKER_PORT" env
  , configDBConn     = conn
  , configOrigins    = map C.pack . words <$> lookup "DBMS_CHECKER_ORIGINS" env
  , configSessionDur = nominalDay * fromIntegral (maybe (7 :: Word) read (lookup "DBMS_SESSION_EXPIRATION_DAYS" env))
  , configFrontendDir = fromMaybe "client/dist" $ lookup "DBMS_CHECKER_FRONTEND_DIR" env
  }
  let keyPath = dataDir </> "jwtKey.json"
      handleErr err = do
        print err
        print ("Trying to create JWT key file..." :: String)
        writeKey keyPath
        readKey keyPath
  keyFromFile <- tryIOError $ readKey keyPath
  myKey <- case keyFromFile of
    Right key -> return key
    Left err -> handleErr err
  run (configPort cfg) (app myKey cfg)
