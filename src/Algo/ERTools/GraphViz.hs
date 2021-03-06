{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Algo.ERTools.GraphViz
  ( showER
  , drawER
  ) where

import Algo.ERTools.Types
import Data.GraphViz hiding (DotGraph)
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Types.Generalised
import qualified Data.GraphViz.Attributes.HTML as H
import Data.GraphViz.Attributes.Complete
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as B

drawER :: ER -> IO B.ByteString
drawER er =
  graphvizWithHandle Neato (er2graph er) Png B.hGetContents

showER :: ER -> Text
showER er = printDotGraph (er2graph er)

er2graph :: ER -> DotGraph Text
er2graph ER{..} = graph (Str "G") $ do
  graphAttrs [Layout Neato, Overlap VoronoiOverlap, Splines SplineEdges]
  mapM_ entNode erEntities
  mapM_ relNode erRels

entNode :: Entity -> DotM Text ()
entNode Entity{..} = do
  node entName (
    [shape BoxShape] <>
    [Peripheries 2 | entStrength == Weak]
    )
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
  mapM_ (\(typ, ent) -> edge relName' ent [HeadLabel $ toLabelValue $ ct2s typ]) relConn
  mapM_ (attrNode relName') relAttrs
  where
    ct2s :: RelType -> Text
    ct2s One = "1"
    ct2s Many = "M"
    relName' = relName <> "=" <> LT.intercalate ":" (map snd relConn)
