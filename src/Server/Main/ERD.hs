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

import Algo.ERTools.Parse
import Algo.ERToFD
import Text.Megaparsec
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BL

erd :: ServerT (BasicCrud "erdId" ERDIdentifier) SessionEnv
erd = postErd
  :<|> erdManip
  where
    erdManip erdId =
              patchErd erdId
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
postErd desc = do
  nid <- getNewId ERDIdentifier
  uId <- asks (userSessionUserId . sessionData)
  let erdiag = ERDiagram {
          id = nid
        , userId = uId
        , diagram = desc
        , derivedFDs = Nothing
        , accepted = NotAccepted
        , validationErrors = []
        }
  bracketDB $ do
    execDB [tutdctx|insert ERDiagram $erdiag|]
  validateErd nid desc

putErd :: ERDIdentifier -> Text -> SessionEnv ERDBody
putErd erdid desc = do
  uId <- asks (userSessionUserId . sessionData)
  bracketDB $ do
    execDB [tutdctx|update ERDiagram where id = $erdid and userId = $uId (
      diagram := $desc, accepted := NotAccepted )|]
  validateErd erdid desc

patchErd :: ERDIdentifier -> AcceptanceState -> SessionEnv ()
patchErd erdid st = bracketDB $ do
  userRole <- asks (userSessionUserRole . sessionData)
  when (userRole /= Teacher) $ throwError err403
  execDB [tutdctx|update ERDiagram where id = $erdid ( accepted := $st )|]

validateErd :: ERDIdentifier -> Text -> SessionEnv ERDBody
validateErd iid desc = do
  case parseER (LT.fromStrict desc) of
    Left err -> do
      let errs = [T.pack $ parseErrorPretty' desc err]
      bracketDB $ do
        execDB [tutdctx|update ERDiagram where id = $iid ( validationErrors := $errs, dfds := Nothing )|]
      return BasicCrudResponseBodyWithAcceptanceAndValidation {
          id = iid,
          description = desc,
          accepted = NotAccepted,
          validationErrors = errs
        }
    Right erd' -> do
      let binfds = Just $ BL.toStrict $ B.encode $ erToFDs erd'
          valerrs = [] :: [Text]
      bracketDB $ do
        execDB [tutdctx|update ERDiagram where id = $iid ( derivedFDs := $binfds, validationErrors := $valerrs )|]
      return BasicCrudResponseBodyWithAcceptanceAndValidation {
          id = iid,
          description = desc,
          accepted = NotAccepted,
          validationErrors = []
        }
