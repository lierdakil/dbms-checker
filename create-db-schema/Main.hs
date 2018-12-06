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

  -- this is some sample code
  -- time <- getCurrentTime
  -- let user = User {
  --       DB.Types.id = UserIdentifier 1
  --     , username = "test"
  --     , group = NoGroup
  --     , saltedPasswordHash = "asd"
  --     , passwordSalt = "asd"
  --     , registrationDate = time
  --     , role = Teacher
  --   }
  -- eCheck $ executeDatabaseContextExpr sessionId conn $
  --   -- either (error . show) Prelude.id $ toInsertExpr [user] "User"
  --   [tutdctx|insert User $user|]
  -- rel <- eCheck $ executeRelationalExpr sessionId conn $
  --   [tutdrel|User|]
  -- -- eCheck $ commit sessionId conn
  -- let user2 = either (error . show) Prelude.id (fromRelation rel) :: [User]
  -- putStrLn (show user2)

  return ()

-- deriving instance Show User
-- fromRelation (Relation _ tupset) = sequence $ map fromTuple $ asList tupset
