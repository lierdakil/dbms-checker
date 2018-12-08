{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Algo.FDTools.Pretty where

import Algo.FDTools.Types
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import qualified Data.Text.Lazy as T

graphToString :: Graph -> T.Text
graphToString = T.intercalate "\n" . map edgeToString . M.toList

edgeToString :: Edge -> T.Text
edgeToString (l, r) = vertexListToString l <> " -> " <> vertexListToString r

vertexListToString :: VertexList -> T.Text
vertexListToString vl
  | S.size vl == 0 = "()"
  | S.size vl == 1 = vertexToString $ head $ S.toList vl
  | otherwise = "(" <> T.intercalate ", " (map vertexToString $ S.toList vl) <> ")"

vertexToString :: Vertex -> T.Text
vertexToString (Vertex parent name) = parent <> "." <> name
