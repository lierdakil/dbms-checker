{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Main.RelSchemas where

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

relschemas :: ServerT (BasicCrud "relschemaId" RelSchemaIdentifier) SessionEnv
relschemas =
        postRelschemas
   :<|> \relschemaId ->
            getRelschemas relschemaId
       :<|> putRelschemas relschemaId

getRelschemas :: RelSchemaIdentifier -> SessionEnv RelSchemaBody
getRelschemas itemId = bracketDB $ do
  (uId, role) <- asks (liftM2 (,) userSessionUserId userSessionUserRole . sessionData)
  let request Student = [tutdrel|RelationalSchema where id = $itemId and userId = $uId|]
      request Teacher = [tutdrel|RelationalSchema where id = $itemId|]
  (items :: [RelationalSchema]) <- fromRelation =<< execDB (request role)
  when (null items) $ throwError err404
  return $ toResponseBody $ head items

postRelschemas :: Text -> SessionEnv RelSchemaBody
postRelschemas desc = do
  nid <- getNewId RelSchemaIdentifier
  uId <- asks (userSessionUserId . sessionData)
  let item = RelationalSchema {
          id = nid
        , userId = uId
        , relations = desc
        , validationErrors = []
        }
  bracketDB $ do
    execDB [tutdctx|insert RelationalSchema $item|]
    commitDB
  validateRelSchema nid desc

putRelschemas :: RelSchemaIdentifier -> Text -> SessionEnv RelSchemaBody
putRelschemas iid desc = do
  uId <- asks (userSessionUserId . sessionData)
  bracketDB $ do
    let verr = [] :: [Text]
    execDB [tutdctx|update RelationalSchema where id = $iid and userId = $uId (
      relations := $desc, validationErrors := $verr )|]
    commitDB
  validateRelSchema iid desc

validateRelSchema :: RelSchemaIdentifier -> Text -> SessionEnv RelSchemaBody
validateRelSchema iid desc = return BasicCrudResponseBodyWithValidation {
    id = iid
  , description = desc
  , validationErrors = []
  }
