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
import Algo.FDTools.Parse
import Algo.FDTools.Util
import Algo.FDTools.Pretty
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Text.Megaparsec
import qualified Data.HashMap.Strict as M
import Data.Maybe
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BL

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
  validateFunDeps nid desc

putFundeps :: FunDepIdentifier -> Text -> SessionEnv FunDepBody
putFundeps fdid desc = do
  uId <- asks (userSessionUserId . sessionData)
  bracketDB $ do
    let verr = [] :: [Text]
    execDB [tutdctx|update FunctionalDependencies where id = $fdid and userId = $uId (
      funDeps := $desc, validationErrors := $verr )|]
  validateFunDeps fdid desc

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
  let efdFromUser = parseGraph $ LT.fromStrict desc
  case efdFromUser of
    Left err -> do
      let errs = [T.pack $ "Ошибка синтаксиса в описании функциональных зависимостей:\n"
               <> parseErrorPretty' desc err]
      bracketDB $ do
        execDB [tutdctx|update FunctionalDependencies where id = $fdid (validationErrors := $errs)|]
      return BasicCrudResponseBodyWithValidation {
        id = fdid
      , description = desc
      , validationErrors = errs
      }
    Right fdFromUser -> do
      -- get user's erd matching given fd
      (erds :: [ERDiagram]) <- bracketDB $
        fromRelation =<<
        execDB [tutdrel|ERDiagram matching (FunctionalDependencies where id = $fdid){userId}|]
      when (null erds) $ throwError err404
      let ERDiagram{derivedFDs, validationErrors} = head erds
      when (isNothing derivedFDs) $ throwError $
        if null validationErrors
        then err500{
          errBody = LTE.encodeUtf8 $ LT.fromStrict $
            "Отсутствует сохранённое описание ФЗ из ERD. Сообщите об этой ошибке администратору"
          }
        else err400{
          errBody = LTE.encodeUtf8 $ LT.fromStrict $
            "Ошибка синтаксиса в описании диаграммы 'сущность-связь':\n" <>
            T.intercalate "\n" validationErrors
          }
      let fdFromER = B.decode $ BL.fromStrict $ fromJust derivedFDs
          extraneous = getUnderiveable fdFromUser fdFromER
          missing = getUnderiveable fdFromER fdFromUser
          errors = map showMissing (M.toList missing) <> map showExtraneous (M.toList extraneous)
          showMissing fd
            = LT.toStrict $ "В модели \"сущность-связь\" присутствует функицональная зависимость\n"
            <> edgeToString fd <> ",\nно она не найдена в переданном списке"
          showExtraneous fd
            = LT.toStrict $ "В модели \"сущность-связь\" отсутствует функицональная зависимость\n"
            <> edgeToString fd <> ",\nно она присутствует в переданном списке"
      bracketDB $ do
        execDB [tutdctx|update FunctionalDependencies where id = $fdid (validationErrors := $errors)|]
      return BasicCrudResponseBodyWithValidation {
          id = fdid
        , description = desc
        , validationErrors = errors
        }
