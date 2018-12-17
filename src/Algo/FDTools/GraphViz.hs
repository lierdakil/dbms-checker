{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Algo.FDTools.GraphViz
  ( printGraph
  , drawGraph
  ) where

import Algo.FDTools.Types
import Algo.FDTools.Pretty
import Data.GraphViz
import Data.GraphViz.Types.Monadic
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Data.GraphViz.Attributes.Complete
import qualified Data.Text.Lazy as T
import qualified Data.ByteString as B

printGraph :: Graph -> T.Text
printGraph edges =
  let g = digraph (Str "G") $ mapM printEdge $ M.toList edges
  in printDotGraph g

drawGraph :: Graph -> IO B.ByteString
drawGraph edges = do
  let
    g = digraph (Str "G") $ do
      graphAttrs [Layout Dot, Splines SplineEdges]
      mapM printEdge $ M.toList edges
  graphvizWithHandle Dot g Png B.hGetContents

printEdge :: Edge -> DotM T.Text ()
printEdge (l, r) | [ln] <- S.toList l = do
  names <- mapM printVertex (S.toList r)
  mapM_ (\n -> vertexToString ln --> n) names
printEdge (l, r) = do
  namesl <- mapM printVertex (S.toList l)
  namesr <- mapM printVertex (S.toList r)
  let name = T.intercalate "," namesl
  node name [shape PointShape]
  mapM_ (\n -> edge n name [ArrowHead noArrow]) namesl
  mapM_ (\n -> name --> n) namesr

printVertex :: Vertex -> DotM T.Text T.Text
printVertex (Vertex p n) = node label [] >> return label
  where label = p <> "." <> n
