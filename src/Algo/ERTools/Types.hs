{-# OPTIONS_GHC -Wall #-}
module Algo.ERTools.Types where

import Data.Map (Map)
import Data.Text.Lazy (Text)

data Attr = Attr
  { attrName :: Text
  , attrIdent :: Bool
  } deriving (Show, Read)
data Entity = Entity
  { entName :: Text
  , entAttrs :: [Attr]
  } deriving (Show, Read)
data RelType = One | Many deriving (Show, Read, Eq)
data RelTypeWStrength = Weak RelType | Strong RelType deriving (Show, Read, Eq)
data Rel = Rel
  { relName :: Text
  , relConn :: [(RelTypeWStrength, Text)]
  , relAttrs :: [Attr]
  } deriving (Show, Read)
data ER = ER
  { erEntities :: Map Text Entity
  , erRels :: [Rel]
  } deriving (Show, Read)
