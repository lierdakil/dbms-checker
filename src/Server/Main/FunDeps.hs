{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Main.FunDeps where

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


fundeps :: ServerT (BasicCrud "fundepId" FunDepIdentifier) SessionEnv
fundeps = postFundeps
   :<|> \fundepId ->
              getFundeps fundepId
         :<|> putFundeps fundepId

postFundeps :: Text -> SessionEnv FunDepBody
postFundeps desc = do
  nid <- getNewId FunDepIdentifier
  uId <- asks (userSessionUserId . sessionData)
  let fds = FunctionalDependencies {
          id = nid
        , userId = uId
        , funDeps = desc
        , validationErrors = []
        }
  bracketDB $ do
    execDB [tutdctx|insert FunctionalDependencies $fds|]
    commitDB
  return $ toResponseBody fds

putFundeps :: FunDepIdentifier -> Text -> SessionEnv FunDepBody
putFundeps fdid desc = do
  uId <- asks (userSessionUserId . sessionData)
  bracketDB $ do
    let verr = [] :: [Text]
    execDB [tutdctx|update FunctionalDependencies where id = $fdid and userId = $uId (
      diagram := $desc, validationErrors := $verr )|]
    commitDB
  return $ toResponseBody FunctionalDependencies {
      id = fdid
    , userId = uId
    , funDeps = desc
    , validationErrors = []
    }

getFundeps :: FunDepIdentifier -> SessionEnv FunDepBody
getFundeps erdId = bracketDB $ do
  (uId, role) <- asks (liftM2 (,) userSessionUserId userSessionUserRole . sessionData)
  let request Student = [tutdrel|FunctionalDependencies where id = $erdId and userId = $uId|]
      request Teacher = [tutdrel|FunctionalDependencies where id = $erdId|]
  (erds :: [FunctionalDependencies]) <- fromRelation =<< execDB (request role)
  when (null erds) $ throwError err404
  return $ toResponseBody $ head erds

validateFunDeps :: FunDepIdentifier -> Text -> SessionEnv FunDepBody
validateFunDeps fdid desc = do
  (erds :: [ERDiagram]) <- bracketDB $ fromRelation =<< (execDB [tutdrel|ERDiagram matching (FunctionalDependencies where id = $fdid){userId}|])
  return undefined
