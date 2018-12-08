{-# OPTIONS_GHC -Wall #-}
module Algo.FDTools.Types where

import Data.Set
import Data.Text.Lazy (Text)

data Vertex = Vertex {vtxAttr :: Text, vtxParent :: Text} deriving (Eq, Ord, Show)
type VertexList = Set Vertex
type Edge = (VertexList, VertexList)
type Graph = Set Edge
