{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.Main.RelSchemas where

import Data.Text (Text)
import Control.Monad.Reader
import Config
import API
import Util
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
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
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
  bracketDB $
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
      bracketDB $
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
            errBody = LTE.encodeUtf8 $ LT.pack (
              "Ошибка синтаксиса в описании функциональных зависимостей:\n" <>
              parseErrorPretty' funDeps err)
            }
      parsedFDs <- either fdSyntaxError pure $ parseGraph $ LT.fromStrict funDeps
      let relSchemaWithFDs = allRelationFDs parsedFDs schemaFromUser
          errors = errMissingFDs parsedFDs relSchemaWithFDs
                <> errExtraFDs parsedFDs schemaFromUser
                <> errNFEKRelations parsedFDs relSchemaWithFDs
                <> errLosslessJoin parsedFDs schemaFromUser
                <> errSamePK schemaFromUser
                <> errConflictingDomain schemaFromUser
      bracketDB $
        execDB [tutdctx|update RelationalSchema where id = $iid (validationErrors := $errors)|]
      return BasicCrudResponseBodyWithValidation {
          id = iid
        , description = desc
        , validationErrors = errors
        }

errMissingFDs :: Graph
              -> S.HashSet (a, M.HashMap VertexList VertexList) -> [Text]
errMissingFDs allFDs relSchemaWithFDs = map showMissing (M.toList missing)
  where
  missing = getUnderiveable allFDs fdsPreservedInRS
  fdsPreservedInRS = S.foldr (\x y -> M.unionWith (<>) (snd x) y) M.empty relSchemaWithFDs
  showMissing fd
    = LT.toStrict $ "В списке функциональных зависимостей присутствует\n"
    <> edgeToString fd <> ",\nно она теряется в реляционной схеме"

data ExtraFDsErr = NoPK Relation | ExtraFDs Relation Graph

errExtraFDs :: Graph -> Relations -> [Text]
errExtraFDs allFDs (Relations rels) = map showExtraneous extraneous
  where
  extraneous = mapMaybe underviableRelFDs $ S.toList rels
  underviableRelFDs rel
    = case flip getUnderiveable allFDs <$> relationFDs rel of
      Nothing -> Just $ NoPK rel
      Just m
        | M.null m -> Nothing
        | otherwise -> Just $ ExtraFDs rel m
  showExtraneous (NoPK rel)
    = LT.toStrict $ "В отношении\n"
    <> relToString rel
    <> "отсутствует первичный ключ"
  showExtraneous (ExtraFDs rel fds)
    = LT.toStrict $ "В отношении\n"
    <> relToString rel
    <> "присутствуют ФЗ\n"
    <> graphToString fds
    <> ",\nкоторых не было в исходном списке"

errNFEKRelations :: Graph -> S.HashSet (Relation, Graph) -> [Text]
errNFEKRelations allFDs relSchemaWithFDs = map showRelNotNFEK relsNotNFEK
  where
  showRelNotNFEK (rel, nonNFEKFDs)
    = LT.toStrict $ "Отношение " <> relToString rel <> " не находится в НФЭК.\n" <>
      "Функциональные зависимости противоречащие НФЭК: \n" <>
      graphToString nonNFEKFDs
  relsNotNFEK = mapMaybe maybeRelInNFEK $ S.toList relSchemaWithFDs
  maybeRelInNFEK rel = case relFDsNotInNFEK efds rel of
    x | M.null x -> Nothing
      | otherwise -> Just (fst rel, x)
  efds = elementaryFDs allFDs

errLosslessJoin :: Graph -> Relations -> [Text]
errLosslessJoin allFDs relSchema = map showNotLosslesJoin notLosslessJoin
  where
  notLosslessJoin = filter pairNotLosslesJoin $ allPairs $ S.toList $ relationsSet relSchema
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
    unionfds = project (S.union a b) allFDs
  showNotLosslesJoin (r1, r2)
    = LT.toStrict $ "Отношения\n"
    <> relToString r1
    <> ",\n"
    <> relToString r2
    <> "\nимеют общие атрибуты, но не являются декомпозицией без потерь."

errSamePK :: Relations -> [Text]
errSamePK relSchema = map showSamePK (M.elems samePK)
  where
  samePK = M.filter (\x -> S.size x > 1)
         $ M.fromListWith (<>) $ map relToPK
         $ S.toList $ relationsSet relSchema
  relToPK rel = (S.filter attributeIsKey $ relationAttributes rel, S.singleton rel)
  showSamePK rl
    = LT.toStrict $ "Отношения\n"
    <> LT.intercalate ",\n" (map relToString $ S.toList rl)
    <> "\nимеют одинаковый первичный ключ."

errConflictingDomain :: Relations -> [Text]
errConflictingDomain relSchema = map differningDomain (M.toList attrsWithDifferentDomain)
  where
  attrMap = allAttrMap relSchema
  attrsWithDifferentDomain = M.filter
      (\(h :| t) -> any (domainDiffersFrom h) t)
      attrMap
  domainDiffersFrom = (/=) `on` (attributeDomain . snd)
  differningDomain (vtx, lst)
    = LT.toStrict $ "Атрибут " <> vertexToString vtx <> " встречается в отношениях\n"
    <> LT.intercalate ",\n" (FL.toList $ NE.map (relToString . fst) lst)
    <> ",\nи имеет в них разные домены."
