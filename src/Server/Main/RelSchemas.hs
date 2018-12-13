{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Algo.RSTools.Parse
import Algo.RSTools.Types
import Algo.RSTools.Util
import Algo.RSTools.Pretty
import Algo.FDTools.Types
import Algo.FDTools.Parse
import Algo.FDTools.Pretty
import Algo.FDTools.Util
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Text.Megaparsec
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Data.Maybe
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.List as L
import Data.Function (on)
import qualified Data.Foldable as FL

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
  validateRelSchema nid desc

putRelschemas :: RelSchemaIdentifier -> Text -> SessionEnv RelSchemaBody
putRelschemas iid desc = do
  uId <- asks (userSessionUserId . sessionData)
  bracketDB $ do
    let verr = [] :: [Text]
    execDB [tutdctx|update RelationalSchema where id = $iid and userId = $uId (
      relations := $desc, validationErrors := $verr )|]
  validateRelSchema iid desc

validateRelSchema :: RelSchemaIdentifier -> Text -> SessionEnv RelSchemaBody
validateRelSchema iid desc = do
  let eschemaFromUser = parseRelations $ LT.fromStrict desc
  case eschemaFromUser of
    Left err -> do
      let errs = [T.pack $ "Ошибка синтаксиса в описании реляционной схемы:\n"
               <> parseErrorPretty' desc err]
      bracketDB $ do
        execDB [tutdctx|update RelationalSchema where id = $iid (validationErrors := $errs)|]
      return BasicCrudResponseBodyWithValidation {
        id = iid
      , description = desc
      , validationErrors = errs
      }
    Right schemaFromUser -> do
      -- get user's fds matching given rel. schema
      (fds :: [FunctionalDependencies]) <- bracketDB $
        fromRelation =<<
        execDB [tutdrel|FunctionalDependencies matching (RelationalSchema where id = $iid){userId}|]
      when (null fds) $ throwError err404
      let FunctionalDependencies{funDeps} = head fds
          fdSyntaxError err = throwError err400{
            errBody = LTE.encodeUtf8 $ LT.pack $ (
              "Ошибка синтаксиса в описании функциональных зависимостей:\n" <>
              parseErrorPretty' funDeps err)
            }
      parsedFDs <- either fdSyntaxError pure $ parseGraph $ LT.fromStrict funDeps
      let allrelfds = allRelationFDs parsedFDs schemaFromUser
          missing = getUnderiveable parsedFDs fdsPreservedInRS
          extraneous = getUnderiveable fdsFromRS parsedFDs
          fdsPreservedInRS = S.foldr (\x y -> M.unionWith (<>) (snd x) y) M.empty $ allrelfds
          fdsFromRS = fdsFromRelSchema schemaFromUser
          attrMap = allAttrMap schemaFromUser
          attrsWithDifferentDomain = M.filter
              (\(h :| t) -> any (domainDiffersFrom h) t)
              attrMap
          domainDiffersFrom = (/=) `on` (attributeDomain . snd)
          samePK
            = M.filter (\x -> S.size x > 1)
            $ M.fromListWith (<>) $ map relToPK $ S.toList $ relationsSet schemaFromUser
          relToPK rel = (pk, S.singleton rel)
            where pk = S.filter attributeIsKey $ relationAttributes rel
          notLosslessJoin = filter pairNotLosslesJoin $ pairs $ S.toList $ relationsSet schemaFromUser
          pairNotLosslesJoin (Relation a', Relation b')
            | S.null intsctn
            = False
            | otherwise
            = not (a `isSubsetOf` cls || b `isSubsetOf` cls)
            where
              a = S.map attributeName a'
              b = S.map attributeName b'
              intsctn = S.intersection a b
              cls = closure intsctn unionfds
              unionfds = project (S.union a b) parsedFDs
          pairs :: [a] -> [(a, a)]
          pairs l = [(x,y) | (x:ys) <- L.tails l, y <- ys]
          efds = elementaryFDs parsedFDs
          relsNotNFEK = mapMaybe maybeRelInNFEK $ S.toList allrelfds
          maybeRelInNFEK rel = case relFDsNotInNFEK efds rel of
            x | M.null x -> Nothing
              | otherwise -> Just (fst rel, x)
          errors = map showMissing (M.toList missing)
                <> map showExtraneous (M.toList extraneous)
                <> map showRelNotNFEK relsNotNFEK
                <> map showNotLosslesJoin notLosslessJoin
                <> map showSamePK (M.elems samePK)
                <> map differningDomain (M.toList attrsWithDifferentDomain)
          differningDomain (vtx, lst)
            = LT.toStrict $ "Атрибут " <> vertexToString vtx <> " встречается в отношениях\n"
            <> LT.intercalate ",\n" (FL.toList $ NE.map (relToString . fst) lst)
            <> ",\nи имеет в них разные домены."
          showNotLosslesJoin (r1, r2)
            = LT.toStrict $ "Отношения\n"
            <> relToString r1
            <> ",\n"
            <> relToString r2
            <> "\nимеют общие атрибуты но не являются декомпозицией без потерь."
          showSamePK rl
            = LT.toStrict $ "Отношения\n"
            <> LT.intercalate ",\n" (map relToString $ S.toList rl)
            <> "\nимеют одинаковый первичный ключ."
          showExtraneous fd
            = LT.toStrict $ "В реляционной схеме присутствует ФЗ\n"
            <> edgeToString fd <> ",\nно она отсутствует в списке функциональных зависимостей"
          showMissing fd
            = LT.toStrict $ "В списке функциональных зависимостей присутствует\n"
            <> edgeToString fd <> ",\nно она теряется в реляционной схеме"
          showRelNotNFEK (rel, nonNFEKFDs)
            = LT.toStrict $ "Отношение " <> relToString rel <> " не находится в НФЭК.\n" <>
              "Функциональные зависимости противоречащие НФЭК: \n" <>
              graphToString nonNFEKFDs
      bracketDB $ do
        execDB [tutdctx|update RelationalSchema where id = $iid (validationErrors := $errors)|]
      return BasicCrudResponseBodyWithValidation {
          id = iid
        , description = desc
        , validationErrors = errors
        }
