{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Algo.FDTools.GraphViz
  ( printGraph
  , drawGraph
  ) where

import Algo.FDTools.Types
import Algo.FDTools.Util
import Data.Maybe
import Data.GraphViz
import Data.GraphViz.Types.Monadic
import qualified Data.Set as S
import Data.GraphViz.Attributes.Complete
import qualified Data.Text.Lazy as T
import qualified Data.ByteString as B

printGraph :: Graph -> T.Text
printGraph edges =
  let g = digraph (Str "G") $ mapM printEdge $ S.toList $ collect edges
  in printDotGraph g

drawGraph :: Graph -> IO B.ByteString
drawGraph edges = do
  let
    g = digraph (Str "G") $ do
      graphAttrs [Layout Dot, Splines SplineEdges]
      mapM printEdge $ S.toList $ collect edges
  graphvizWithHandle Dot g Png B.hGetContents

printEdge :: Edge -> DotM T.Text ()
printEdge (l, r) | S.size l == 1
                 , ln <- S.elemAt 0 l = do
  names <- catMaybes <$> mapM printVertex (S.toList r)
  mapM_ (\n -> vtxName ln --> n) names
  return ()
printEdge (l, r) = do
  namesl <- catMaybes <$> mapM printVertex (S.toList l)
  namesr <- catMaybes <$> mapM printVertex (S.toList r)
  let name = T.intercalate "," namesl
  node name [shape PointShape]
  mapM_ (\n -> edge n name [ArrowHead noArrow]) namesl
  mapM_ (\n -> name --> n) namesr
  return ()

printVertex :: Vertex -> DotM T.Text (Maybe T.Text)
printVertex (Vertex n)
  | n `elem` ["∅","θ","Θ","0"] = return Nothing
  | otherwise = node n [] >> return (Just n)
