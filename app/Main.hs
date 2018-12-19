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

import System.IO.Error

import ProjectM36.Client

main :: IO ()
main = do
  env <- getEnvironment
  let connInfo = InProcessConnectionInfo (CrashSafePersistence "data/database") emptyNotificationCallback []
  conn <- either (error . show) id <$> connectProjectM36 connInfo
  let cfg = Config {
    configPort       = maybe 8081 read $ lookup "DBMS_CHECKER_PORT" env
  , configDBConn     = conn
  , configOrigins    = words <$> lookup "DBMS_CHECKER_ORIGINS" env
  , configSessionDur = nominalDay * fromIntegral (maybe (7 :: Word) read (lookup "DBMS_SESSION_EXPIRATION_DAYS" env))
  }
  let keyPath = "data/jwtKey.json"
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
