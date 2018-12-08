{-# LANGUAGE DeriveAnyClass, DeriveGeneric, StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Algo.FDTools.Types where

import Data.HashSet as S
import Data.HashMap.Strict as M
import Data.Text.Lazy (Text)
import Data.Hashable
import GHC.Generics
import Data.Binary

data Vertex = Vertex {vtxAttr :: Text, vtxParent :: Text}
  deriving (Eq, Ord, Show, Generic, Hashable, Binary)
type VertexList = HashSet Vertex
type Edge = (VertexList, VertexList)
type Graph = HashMap VertexList VertexList

instance Binary Graph where
  put = put . M.toList
  get = M.fromList <$> get
instance Binary VertexList where
  put = put . S.toList
  get = S.fromList <$> get
