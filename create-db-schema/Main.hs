{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import ProjectM36.Client
import API.TH
import DB.TypeLists
import DB.Instances ()
import ProjectM36.Tupleable
import qualified Data.Text as T
import Language.Haskell.TH.Syntax
import Crypto.KDF.BCrypt
import Data.UUID.V1
import DB.Types
import Data.Time
import Data.ByteString (ByteString)
import TutorialD.QQ

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

  -- this creates the domains and relations, but not the constraints
  _ <- mapM (eCheck . executeDatabaseContextExpr sessionId conn) $
      $(foldr (\x acc -> [|toAddTypeExpr $(makeProxy x) : $acc|]) [|[]|] domains) ++
      $(foldr (\x acc -> [|toDefineExpr $(makeProxy x) (T.pack $ nameBase x) : $acc|]) [|[]|] DB.TypeLists.relations) ++
      []

  time <- getCurrentTime
  Just uuid <- nextUUID
  pwd <- hashPassword 12 ("password" :: ByteString)
  let user = User {
        DB.Types.id = UserIdentifier uuid
      , username = "test"
      , group = NoGroup
      , saltedPasswordHash = pwd
      , registrationDate = time
      , role = Teacher
    }
  eCheck $ executeDatabaseContextExpr sessionId conn $ [tutdctx|insert User $user|]
  eCheck $ commit sessionId conn

  return ()
