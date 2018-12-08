{-# OPTIONS_GHC -Wall #-}
module Algo.ERTools.Types where

import Data.HashMap.Strict (HashMap)
import Data.Text.Lazy (Text)

data EntStrength = Strong | Weak deriving (Show, Read, Eq)
data AttrParent = AttrParentEnt Text | AttrParentRel Text
  deriving (Show, Read, Eq)
data Attr = Attr
  { attrParent :: AttrParent
  , attrName :: Text
  , attrIdent :: Bool
  } deriving (Show, Read, Eq)
data Entity = Entity
  { entName :: Text
  , entAttrs :: [Attr]
  , entStrength :: EntStrength
  } deriving (Show, Read)
data RelType = One | Many deriving (Show, Read, Eq)
data Rel = Rel
  { relName :: Text
  , relConn :: [(RelType, Text)]
  , relAttrs :: [Attr]
  } deriving (Show, Read)
data ER = ER
  { erEntities :: HashMap Text Entity
  , erRels :: [Rel]
  } deriving (Show, Read)
