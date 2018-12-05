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

-- {-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
-- module Main where
--
-- import ProjectM36.Client
-- import ProjectM36.Relation.Show.Term
-- import GHC.Generics
-- import Data.Text
-- import Data.Binary
-- import Control.DeepSeq
-- import qualified Data.Map as M
-- import qualified Data.Text.IO as TIO
-- import Data.Proxy
-- import TutorialD.QQ
--
-- data Hair = Bald | Brown | Blond | OtherColor Text
--    deriving (Generic, Show, Eq, Binary, NFData, Atomable)
--
-- main :: IO ()
-- main = do
--  --connect to the database
--   let connInfo = InProcessConnectionInfo NoPersistence emptyNotificationCallback []
--       eCheck v = do
--         x <- v
--         case x of
--           Left err -> error (show err)
--           Right x' -> pure x'
--   conn <- eCheck $ connectProjectM36 connInfo
--
--   --create a database session at the default branch of the fresh database
--   sessionId <- eCheck $ createSessionAtHead conn "master"
--
--   --create the data type in the database context
--   eCheck $ executeDatabaseContextExpr sessionId conn (toAddTypeExpr (Proxy :: Proxy Hair))
--
--   --create a relation with the new Hair AtomType
--   let blond = Blond
--       grey = OtherColor "Grey"
--   eCheck $ executeDatabaseContextExpr sessionId conn
--     [tutdctx|people := relation{
--       tuple{hair $blond, name "Colin"},
--       tuple{hair $grey, name "Greg"}
--     }|]
--   -- eCheck $ executeDatabaseContextExpr sessionId conn (Assign "people" (MakeRelationFromExprs Nothing [
--   --           TupleExpr (M.fromList [("hair", blond), ("name", NakedAtomExpr (TextAtom "Colin"))])]))
--
--   -- let restrictionPredicate = AttributeEqualityPredicate "hair" blond
--   -- peopleRel <- eCheck $ executeRelationalExpr sessionId conn (Restrict restrictionPredicate (RelationVariable "people" ()))
--   peopleRel <- eCheck $ executeRelationalExpr sessionId conn
--     [tutdrel|people where hair = $grey|]
--
--   TIO.putStrLn (showRelation peopleRel)
