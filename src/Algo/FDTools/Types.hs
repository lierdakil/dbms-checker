{-# OPTIONS_GHC -Wall #-}
module Algo.FDTools.Types where

import Data.Set
import Data.Text.Lazy (Text)

newtype Vertex = Vertex {vtxName :: Text} deriving (Eq, Ord, Show)
type VertexList = Set Vertex
type Edge = (VertexList, VertexList)
type Graph = Set Edge
