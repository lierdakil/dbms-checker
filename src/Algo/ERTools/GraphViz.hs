{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Algo.ERTools.GraphViz
  ( showER
  , drawER
  ) where

import Algo.ERTools.Types
import Data.GraphViz
import Data.GraphViz.Types.Monadic
import qualified Data.GraphViz.Attributes.HTML as H
import Data.GraphViz.Attributes.Complete
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as B

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe (return ())

drawER :: ER -> IO B.ByteString
drawER ER{..} = do
  let
    g = graph (Str "G") $ do
      graphAttrs [Layout Neato, Overlap VoronoiOverlap, Splines SplineEdges]
      mapM_ entNode erEntities
      mapM_ relNode erRels
  graphvizWithHandle Neato g Png B.hGetContents

showER :: ER -> Text
showER ER{..} =
  let
    g = graph (Str "G") $ do
      graphAttrs [Layout Neato, Overlap VoronoiOverlap, Splines SplineEdges]
      mapM_ entNode erEntities
      mapM_ relNode erRels
  in printDotGraph g

entNode :: Entity -> DotM Text ()
entNode Entity{..} = do
  node entName [shape BoxShape]
  mapM_ (attrNode entName) entAttrs

attrNode :: Text -> Attr -> DotM Text ()
attrNode p Attr{..} = do
  let nid = p <> ":" <> attrName
      lab | attrIdent = toLabel [H.Format H.Underline [H.Str attrName]]
          | otherwise = toLabel attrName
  node nid [lab]
  p --> nid

relNode :: Rel -> DotM Text ()
relNode Rel{..} = do
  node relName' [shape DiamondShape, textLabel relName]
  mapM_ (\(typ, ent) -> edge relName' ent ([HeadLabel $ toLabelValue $ cts2s typ] <> cts2a typ)) relConn
  mapM_ (attrNode relName') relAttrs
  where
    cts2a :: RelTypeWStrength -> [Attribute]
    cts2a (Strong _) = []
    cts2a (Weak _) = [ArrowHead normal, edgeEnds Forward]
    cts2s :: RelTypeWStrength -> Text
    cts2s (Strong x) = ct2s x
    cts2s (Weak x) = ct2s x
    ct2s :: RelType -> Text
    ct2s One = "1"
    ct2s Many = "M"
    relName' = relName <> "=" <> LT.intercalate ":" (map snd relConn)
