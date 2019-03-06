{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import ProjectM36.Client
import API.TH
import DB.TypeLists
import DB.Instances ()
import ProjectM36.Tupleable
import qualified Data.Text as T
import Language.Haskell.TH.Syntax
import Crypto.KDF.BCrypt
import Data.UUID.V1
import DB.Types
import Data.Time
import Data.ByteString (ByteString)
import TutorialD.QQ
import System.Environment
import System.FilePath
import Data.Maybe

main :: IO ()
main = do
  env <- getEnvironment
  let dataDir = fromMaybe "data" $ lookup "DBMS_CHECKER_DATA_DIR" env
      connInfo = InProcessConnectionInfo (CrashSafePersistence $ dataDir </> "database") emptyNotificationCallback []
      eCheck v = do
        x <- v
        case x of
          Left err -> error (show err)
          Right x' -> pure x'
  conn <- eCheck $ connectProjectM36 connInfo

  sessionId <- eCheck $ createSessionAtHead conn "master"

  -- this creates the domains and relations, but not the constraints
  mapM_ (eCheck . executeDatabaseContextExpr sessionId conn) $
      $(foldr (\x acc -> [|toAddTypeExpr $(makeProxy x) : $acc|]) [|[]|] domains) ++
      $(foldr (\x acc -> [|toDefineExpr $(makeProxy x) (T.pack $ nameBase x) : $acc|]) [|[]|] DB.TypeLists.relations) ++
      []

  let topics = [
          "Автопрокат"
        , "Автосалон"
        , "Агентство недвижимости"
        , "Аптека"
        , "Банк"
        , "Библиотека"
        , "Боксёрский клуб"
        , "Боулинг-клуб"
        , "Бюро по трудоустройству"
        , "Гостиница"
        , "Интернет-магазин косметики"
        , "Интернет-магазин музыки"
        , "Интернет-магазин обуви"
        , "Интернет-магазин электротехники"
        , "Книжное издательство"
        , "Книжный магазин"
        , "Кофейня"
        , "Курьерская служба"
        , "Ломбард"
        , "Магазин дверей"
        , "Магазин живописи"
        , "Магазин игрушек"
        , "Магазин компютерных комплектующих"
        , "Магазин компьютерных игр"
        , "Магазин мебели"
        , "Магазин музыкальных инструментов"
        , "Магазин одежды"
        , "Магазин подарков"
        , "Магазин спортивных товаров"
        , "Магазин сувениров"
        , "Онлайн-кинотеатр"
        , "Онлайн-магазин посуды"
        , "Парикмахерская"
        , "Платная поликлиника"
        , "Продуктовый магазин"
        , "Рекламное агентство"
        , "Ресторан"
        , "Служба такси"
        , "Торговый центр (аренда торговых площадей)"
        , "Туристическая фирма"
        , "Фитнес-клуб"
        , "Цветочный магазин"
        , "Школа английского языка"
        ]
  trels <- mapM (\t -> nextUUID >>= \(Just u) -> return $ PredefinedTopic (PredefinedTopicIdentifier u) t) topics
  eCheck $ executeDatabaseContextExpr sessionId conn [tutdctx|insert PredefinedTopic $trels|]
  eCheck $ commit sessionId conn

  return ()
