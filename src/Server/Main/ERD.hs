{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Main.ERD where

import Data.Text (Text)
import Control.Monad.Reader
import Config
import API
import API.Types
import DB.Types
import Servant
import DB.Instances ()
import DB.Accessor
import DB.Utils
import TutorialD.QQ

erd :: ServerT (BasicCrud "erdId" ERDIdentifier) SessionEnv
erd = postErd
  :<|> erdManip
  where
    erdManip erdId =
              renderErd erdId
         :<|> patchErd erdId
         :<|> getErd erdId
         :<|> putErd erdId

getErd :: ERDIdentifier -> SessionEnv ERDBody
getErd erdId = bracketDB $ do
  (uId, role) <- asks (liftM2 (,) userSessionUserId userSessionUserRole . sessionData)
  let request Student = [tutdrel|ERDiagram where id = $erdId and userId = $uId|]
      request Teacher = [tutdrel|ERDiagram where id = $erdId|]
  (erds :: [ERDiagram]) <- fromRelation =<< execDB (request role)
  when (null erds) $ throwError err404
  return $ toResponseBody $ head erds

postErd :: Text -> SessionEnv ERDBody
postErd desc = bracketDB $ do
  nid <- getNewId ERDIdentifier
  uId <- asks (userSessionUserId . sessionData)
  let erdiag = ERDiagram {
          id = nid
        , userId = uId
        , diagram = desc
        , accepted = NotAccepted
        }
  execDB [tutdctx|insert ERDiagram $erdiag|]
  commitDB
  return $ toResponseBody erdiag

putErd :: ERDIdentifier -> Text -> SessionEnv ERDBody
putErd erdid desc = bracketDB $ do
  uId <- asks (userSessionUserId . sessionData)
  execDB [tutdctx|update ERDiagram where id = $erdid and userId = $uId (
    diagram := $desc, accepted := NotAccepted )|]
  commitDB
  return $ toResponseBody ERDiagram {
      id = erdid
    , userId = uId
    , diagram = desc
    , accepted = NotAccepted
    }

patchErd :: ERDIdentifier -> AcceptanceState -> SessionEnv ()
patchErd erdid st = bracketDB $ do
  userRole <- asks (userSessionUserRole . sessionData)
  when (userRole /= Teacher) $ throwError err403
  execDB [tutdctx|update ERDiagram where id = $erdid ( accepted := $st )|]
  commitDB

renderErd :: ERDIdentifier -> SessionEnv FileData
renderErd = undefined
