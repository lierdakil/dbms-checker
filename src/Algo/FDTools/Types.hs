{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Algo.FDTools.Types where

import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Data.Text.Lazy (Text)
import Data.Hashable
import GHC.Generics

data Vertex = Vertex {vtxAttr :: Text, vtxParent :: Text}
  deriving (Eq, Ord, Show, Generic, Hashable)
type VertexList = HashSet Vertex
type Edge = (VertexList, VertexList)
type Graph = HashMap VertexList VertexList
