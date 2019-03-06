{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE DuplicateRecordFields, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-orphans #-}
module Main where

import ProjectM36.Client hiding (Group)
import DB.Instances ()
import qualified Data.Text as T
import Crypto.KDF.BCrypt
import Data.UUID.V1
import DB.Types
import Data.Time
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy.Encoding as LE
import TutorialD.QQ
import System.Environment
import System.FilePath
import Data.Maybe

deriving instance Read Role

main :: IO ()
main = do
  env <- getEnvironment
  let dataDir = fromMaybe "data" $ lookup "DBMS_CHECKER_DATA_DIR" env
      connInfo = InProcessConnectionInfo (CrashSafePersistence $ dataDir </> "database") emptyNotificationCallback []
      eCheck v = do
        x <- v
        case x of
          Left err -> error (show err)
          Right x' -> pure x'
  conn <- eCheck $ connectProjectM36 connInfo

  sessionId <- eCheck $ createSessionAtHead conn "master"

  time <- getCurrentTime
  username:email:role:group <- getArgs
  password <- BL.toStrict . LE.encodeUtf8 <$> LT.getLine
  Just uuid <- nextUUID
  pwd <- hashPassword 12 password
  let user = User {
        DB.Types.id = UserIdentifier uuid
      , username = T.pack username
      , email = T.pack email
      , group = if null group then NoGroup else Group (T.pack $ head group)
      , saltedPasswordHash = pwd
      , registrationDate = time
      , role = read role
    }
  eCheck $ executeDatabaseContextExpr sessionId conn [tutdctx|insert User $user|]
  eCheck $ commit sessionId conn

  return ()
