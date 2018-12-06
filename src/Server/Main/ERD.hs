{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Main.ERD where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Set as S
import Control.Monad.Reader
import Config
import API
import API.Types
import DB.Types
import Servant
import Control.Monad.Error.Class
import Data.Monoid ((<>))
import Crypto.Hash (hash, Digest, SHA1)
import Data.Char (isAlphaNum)
import Control.Exception
import System.IO.Error
import API
import API.Types
import DB.Types
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
getErd erdId = do
  (uId, role) <- asks (liftM2 (,) userSessionUserId userSessionUserRole . sessionData)
  let request Student = [tutdrel|ERDiagram where id = $erdId and userId = $uId|]
      request Teacher = [tutdrel|ERDiagram where id = $erdId|]
  (erds :: [ERDiagram]) <- fromRelation =<< execDB (request role)
  when (null erds) $ throwError err404
  return $ toResponseBody $ head erds

postErd :: Text -> SessionEnv ERDBody
postErd desc = do
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
putErd erdid desc = do
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
patchErd erdid st = do
  userRole <- asks (userSessionUserRole . sessionData)
  when (userRole /= Teacher) $ throwError err403
  execDB [tutdctx|update ERDiagram where id = $erdid ( accepted := $st )|]
  commitDB

renderErd :: ERDIdentifier -> SessionEnv FileData
renderErd = undefined
