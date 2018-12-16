{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Main.PhysSchemas where

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
import Algo.SQLTools.Parse
import Algo.SQLTools.Types
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
import qualified Data.List as L
import Data.Function (on)
import qualified Data.Foldable as FL

sqlschemas :: ServerT (BasicCrud "sqlschemaId" PhysSchemaIdentifier) SessionEnv
sqlschemas = postSqlschemas
   :<|> \sqlSchemaId ->
              patchSqlschemas sqlSchemaId
         :<|> getSqlschemas sqlSchemaId
         :<|> putSqlschemas sqlSchemaId

getSqlschemas :: PhysSchemaIdentifier -> SessionEnv PhysSchemaBody
getSqlschemas itemId = bracketDB $ do
  (uId, role) <- asks (liftM2 (,) userSessionUserId userSessionUserRole . sessionData)
  let request Student = [tutdrel|PhysicalSchema where id = $itemId and userId = $uId|]
      request Teacher = [tutdrel|PhysicalSchema where id = $itemId|]
  (items :: [PhysicalSchema]) <- fromRelation =<< execDB (request role)
  when (null items) $ throwError err404
  return $ toResponseBody $ head items

postSqlschemas :: Text -> SessionEnv PhysSchemaBody
postSqlschemas desc = do
  nid <- getNewId PhysSchemaIdentifier
  uId <- asks (userSessionUserId . sessionData)
  let fds = PhysicalSchema {
          id = nid
        , userId = uId
        , schemaSQL = desc
        , accepted = NotAccepted
        , validationErrors = []
        }
  bracketDB $ do
    execDB [tutdctx|insert PhysicalSchema $fds|]
  validatePhysSchema nid desc

putSqlschemas :: PhysSchemaIdentifier -> Text -> SessionEnv PhysSchemaBody
putSqlschemas iid desc = do
  uId <- asks (userSessionUserId . sessionData)
  bracketDB $ do
    let verr = [] :: [Text]
        acc = NotAccepted
    execDB [tutdctx|update PhysicalSchema where id = $iid and userId = $uId (
      schemaSQL := $desc, validationErrors := $verr, accepted := $acc )|]
  validatePhysSchema iid desc

patchSqlschemas :: PhysSchemaIdentifier -> AcceptanceState -> SessionEnv ()
patchSqlschemas iid st = bracketDB $ do
  userRole <- asks (userSessionUserRole . sessionData)
  when (userRole /= Teacher) $ throwError err403
  execDB [tutdctx|update PhysicalSchema where id = $iid ( accepted := $st )|]

-- TODO
validatePhysSchema :: PhysSchemaIdentifier -> Text -> SessionEnv PhysSchemaBody
validatePhysSchema iid desc =
  case parseTables $ LT.fromStrict desc of
    Left err -> do
      let errs = [T.pack $ "Ошибка синтаксиса в описании физической схемы:\n"
               <> parseErrorPretty' desc err]
      bracketDB $ do
        execDB [tutdctx|update PhysicalSchema where id = $iid (validationErrors := $errs)|]
      return BasicCrudResponseBodyWithAcceptanceAndValidation {
        id = iid
      , description = desc
      , validationErrors = errs
      , accepted = NotAccepted
      }
    Right schemaFromUser -> do
      let tablesMap = M.fromList $ map (\t -> (tableName t, (colMap t, t))) schemaFromUser
          colMap Table{..} = M.fromList $ map (\c -> (columnName c, c)) tableCols
      return BasicCrudResponseBodyWithAcceptanceAndValidation {
        id = iid
      , description = desc
      , validationErrors = checkDuplicateTables schemaFromUser
                        <> concatMap checkDuplicateColumns schemaFromUser
                        <> concatMap (checkForeignKeys tablesMap) schemaFromUser
      , accepted = NotAccepted
      }

checkDuplicateTables :: [Table] -> [Text]
checkDuplicateTables schema =
  map showDupTables $ filter (uncurry ((==) `on` tableName)) $ allPairs schema
  where
  showDupTables (a, _) = LT.toStrict $ "Две таблицы с одинаковым именем " <> tableName a

checkDuplicateColumns :: Table -> [Text]
checkDuplicateColumns Table{..} =
  map showDupCol $ filter (uncurry ((==) `on` columnName)) $ allPairs tableCols
  where
  showDupCol (a, _) = LT.toStrict $
    "В таблице " <> tableName <> " две колонки с именем " <> columnName a

checkForeignKeys :: M.HashMap LT.Text (M.HashMap LT.Text Column, b)
                    -> Table -> [Text]
checkForeignKeys tm Table{..}
  = concatMap checkFKAttr tableAttrs
  where
  err desc =
    LT.toStrict $ "Неверное ограничение внешнего ключа в таблице " <> tableName <> ":\n" <> desc
  checkFKAttr TableForeignKey{..} =
    if length fkCols /= length fkRefCols
    then [err "количество столбцов не совпадает"]
    else
      case M.lookup fkRefTbl tm of
        Nothing -> [err $ "таблица " <> fkRefTbl <> " не существует"]
        Just (rt, _) -> concatMap (checkDomains fkRefTbl rt) (zip fkCols fkRefCols)
  checkFKAttr _ = []
  checkDomains refTblN refTbl (colN, refN) = catMaybes
    [ checkSelfColExists
    , checkRefColExists
    , join $ liftM2 checkColDomain selfCol refCol
    ]
    where
      selfCol = M.lookup colN selfTbl
      refCol = M.lookup refN refTbl
      checkSelfColExists =
        if isNothing selfCol
        then Just $ missingColErr tableName colN
        else Nothing
      checkRefColExists =
        if isNothing refCol
        then Just $ missingColErr refTblN refN
        else Nothing
      checkColDomain sc rc =
        if ((/=) `on` columnType) sc rc
        then Just $ err $ "у столбцов " <> tableName <> "." <> colN <> " и " <> refTblN <> "." <> refN <> " разные типы"
        else Nothing
  missingColErr tn cn = err $ "столбец " <> tn <> "." <> cn <> " не существует"
  Just (selfTbl, _) = M.lookup tableName tm
