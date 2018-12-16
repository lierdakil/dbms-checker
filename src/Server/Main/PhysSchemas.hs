{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Algo.RSTools.Pretty
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Text.Megaparsec
import qualified Data.HashSet as S
import qualified Data.MultiSet as MS
import qualified Data.HashMap.Strict as M
import Data.Maybe
import qualified Data.List as L
import Data.Function (on)

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
      (rss :: [RelationalSchema]) <- bracketDB $
        fromRelation =<<
        execDB [tutdrel|RelationalSchema matching (PhysicalSchema where id = $iid){userId}|]
      when (null rss) $ throwError err404
      let RelationalSchema{relations} = head rss
          rsSyntaxError err = throwError err400{
            errBody = LTE.encodeUtf8 $ LT.pack $ (
              "Ошибка синтаксиса в описании реляционной схемы:\n" <>
              parseErrorPretty' relations err)
            }
      rs <- either rsSyntaxError pure $ parseRelations $ LT.fromStrict relations
      let rsType = relationalSchemaType rs
          psType = relationalSchemaTypeFromPhysical schemaFromUser
      return BasicCrudResponseBodyWithAcceptanceAndValidation {
        id = iid
      , description = desc
      , validationErrors = checkDuplicateTables schemaFromUser
                        <> concatMap checkDuplicateColumns schemaFromUser
                        <> concatMap (checkForeignKeys tablesMap) schemaFromUser
                        <> concatMap checkPrimaryKeys schemaFromUser
                        <> map relationsMissingInSchema (MS.toList $ MS.difference rsType psType)
                        <> map relationsExtraneousInSchema (MS.toList $ MS.difference psType rsType)
      , accepted = NotAccepted
      }

relationsMissingInSchema :: MS.MultiSet (Bool, Domain) -> Text
relationsMissingInSchema rel = LT.toStrict $ "Отношение вида\n(" <> relDesc rel <> ")\nприсутствуте в реляционной схеме, но отсутствует в физической"

relationsExtraneousInSchema :: MS.MultiSet (Bool, Domain) -> Text
relationsExtraneousInSchema rel = LT.toStrict $ "Отношение вида\n(" <> relDesc rel <> ")\nприсутствуте в физической схеме, но отсутствует в реляционной"

relDesc :: MS.MultiSet (Bool, Domain) -> LT.Text
relDesc rel = LT.intercalate ", " $ map (\(isPK, dom) -> (if isPK then "*" else "") <> domainToString dom) $ MS.toList rel

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

checkPrimaryKeys :: Table -> [Text]
checkPrimaryKeys Table{..}
  | [] <- filter isPKCol tableCols
  , [] <- filter isPKAttr tableAttrs
  = [LT.toStrict $ "В таблице " <> tableName <> " отсутствует первичный ключ"]
  | _:_ <- filter isPKCol tableCols
  , _:_ <- filter isPKAttr tableAttrs
  = [LT.toStrict $ "В таблице " <> tableName <> " присутствуют объявления первичного ключа для отдельных столбцов и для кортежа"]
  | _:_:_ <- filter isPKCol tableCols
  = [LT.toStrict $ "В таблице " <> tableName <> " присутствуют множественные объявления первичного ключа для отдельных столбцов"]
  | _:_:_ <- filter isPKAttr tableAttrs
  = [LT.toStrict $ "В таблице " <> tableName <> " присутствуют множественные объявления первичного ключа для кортежа"]
  | otherwise = []
  where isPKCol Column{..} = PrimaryKey `elem` columnAttrs
        isPKAttr TablePrimaryKey{} = True
        isPKAttr _ = False

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

relationalSchemaType :: Relations -> MS.MultiSet (MS.MultiSet (Bool, Domain))
relationalSchemaType (Relations rels) = MS.fromList $ map relationType $ S.toList rels
  where
  relationType (Relation attrs) = MS.fromList $ map (\Attribute{..} -> (attributeIsKey, attributeDomain)) $ S.toList attrs

relationalSchemaTypeFromPhysical :: [Table] -> MS.MultiSet (MS.MultiSet (Bool, Domain))
relationalSchemaTypeFromPhysical = MS.fromList . map tableToRelType
  where
  tableToRelType Table{..} =
    let primaryKeyCols = fromMaybe [] $ unPrimaryKey <$> L.find isPK tableAttrs
        unPrimaryKey (TablePrimaryKey attrs) = attrs
        unPrimaryKey _ = error "This should not happen"
        isPK (TablePrimaryKey _) = True
        isPK _ = False
    in MS.fromList $ map (colToRelT primaryKeyCols) tableCols
  colToRelT [] Column{..} = (PrimaryKey `elem` columnAttrs, dataTypeToDomain columnType)
  colToRelT pk Column{..} = (columnName `elem` pk, dataTypeToDomain columnType)
  dataTypeToDomain (TypeChar _) = DomainString
  dataTypeToDomain (TypeVarChar _) = DomainString
  dataTypeToDomain (TypeText _) = DomainText
  dataTypeToDomain (TypeInt _ attrs)
    | Unsigned `elem` attrs = DomainNumber Natural
    | otherwise = DomainNumber Whole
  dataTypeToDomain (TypeRational _ _ _) = DomainNumber Rational
  dataTypeToDomain (TypeFloat _ _) = DomainNumber Rational
  dataTypeToDomain (TypeDate) = DomainDate
  dataTypeToDomain (TypeTime) = DomainTime
  dataTypeToDomain (TypeDateTime) = DomainDateTime
  dataTypeToDomain (TypeEnum vars) = DomainEnum $ S.fromList vars
