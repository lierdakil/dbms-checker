{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Algo.FDTools.Pretty where

import Algo.FDTools.Types
import qualified Data.Set as S
import qualified Data.Text.Lazy as T

graphToString :: Graph -> T.Text
graphToString = T.intercalate "\n" . map edgeToString . S.toList

edgeToString :: Edge -> T.Text
edgeToString (l, r) = vertexListToString l <> " -> " <> vertexListToString r

vertexListToString :: VertexList -> T.Text
vertexListToString vl
  | S.size vl == 0 = "()"
  | S.size vl == 1 = vertexToString $ S.elemAt 0 vl
  | otherwise = "(" <> T.intercalate ", " (map vertexToString $ S.toList vl) <> ")"

vertexToString :: Vertex -> T.Text
vertexToString (Vertex parent name) = parent <> "." <> name
