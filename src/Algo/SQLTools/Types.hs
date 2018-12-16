{-# LANGUAGE DeriveAnyClass, DeriveGeneric, StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Algo.SQLTools.Types where

import Data.Text.Lazy (Text)
import Algo.FDTools.Types
import Data.Hashable
import GHC.Generics

data NumberAttr = Unsigned | ZeroFill | AutoIncrement | Width Word
    deriving (Eq, Generic, Hashable)
data DataType = TypeChar Word
              | TypeVarChar Word
              | TypeText Word
              | TypeInt Word [NumberAttr]
              | TypeRational Word Word [NumberAttr]
              | TypeFloat Word [NumberAttr]
              | TypeDate
              | TypeTime
              | TypeDateTime
              | TypeEnum [Text]
    deriving (Eq, Generic, Hashable)
data ColumnAttr = NotNull | PrimaryKey
    deriving (Eq, Generic, Hashable)
data Column = Column {
      columnName :: Vertex
    , columnType :: DataType
    , columnAttrs :: [ColumnAttr]
    } deriving (Eq, Generic, Hashable)
data FKAction = NoAction | ActionSetNull | ActionRestrict | ActionCascade
    deriving (Eq, Generic, Hashable)
data TableAttr = TablePrimaryKey [Vertex] | TableForeignKey {
    fkCols :: [Vertex]
  , fkRefCols :: [Vertex]
  , fkOnUpdate :: FKAction
  , fkOnDelete :: FKAction
  } deriving (Eq, Generic, Hashable)
data Table = Table {
      tableName :: Text
    , tableCols :: [Column]
    , tableAttrs :: [TableAttr] }
    deriving (Eq, Generic, Hashable)
