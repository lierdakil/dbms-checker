{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
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

main :: IO ()
main = do
  let connInfo = InProcessConnectionInfo (CrashSafePersistence "data/database") emptyNotificationCallback []
      eCheck v = do
        x <- v
        case x of
          Left err -> error (show err)
          Right x' -> pure x'
  conn <- eCheck $ connectProjectM36 connInfo

  sessionId <- eCheck $ createSessionAtHead conn "master"

  time <- getCurrentTime
  username:email:group <- getArgs
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
      , role = Student
    }
  eCheck $ executeDatabaseContextExpr sessionId conn [tutdctx|insert User $user|]
  eCheck $ commit sessionId conn

  return ()
