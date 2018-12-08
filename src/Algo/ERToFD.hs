{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Algo.ERToFD where

import Algo.ERTools.Types
import Algo.FDTools.Types
import Algo.FDTools.Util
import Data.Maybe
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S

erToFDs :: ER -> Graph
erToFDs ER{..} = collect . nontrivial . S.fromList $
  entityFDs ++ relFDs
  where
    entityFDs = concatMap (entityFD . snd) $ M.toList erEntities
    entityFD x= map (--> attrs2vtx x (entAttrs x)) $ keysFromEnt x
    keysFromEnt :: Entity -> [S.Set Vertex]
    keysFromEnt e
      | entStrength e == Weak
      = concatMap (map (entKeysNative e <>) . keysFromEnt) (relsEntsWith1 e)
      | otherwise -- Strong
      = [entKeysNative e]
    relsEntsWith1 x = filter ((/=entName x) . entName) $ concatMap get1 (relsWith x)
    get1 Rel{..} = mapMaybe (findEntInConnOfType One) relConn
    relsWith y = filter (has y) erRels
    has y Rel{..} = any ((== entName y) . snd) relConn
    findEntInConnOfType t (t', n)
      | t == t' = M.lookup n erEntities
      | otherwise = Nothing
    entKeysNative :: Entity -> S.Set Vertex
    entKeysNative x
      | null aas = attrs2vtx x [Attr (AttrParentEnt $ entName x) "id" True]
      | otherwise = attrs2vtx x kas
      where kas = keyAttrs aas
            aas = entAttrs x
    relFDs = concatMap relFD erRels
    relFD Rel{..}
      | null $ ctKeys Many
      = oneToOne
      | otherwise
      = ctKeys Many --> rsOrNull : oneToOne
      where
        oneToOne = concatMap (\x -> map (--> rs) $ keysFromEnt x) (ctf One)
        rs = S.unions [ctKeys One, attrs2vtx' relName relAttrs]
        rsOrNull | S.null rs = S.singleton (Vertex relName "Θ")
                 | otherwise = rs
        ctf t = mapMaybe (findEntInConnOfType t) relConn
        ctKeys t = getKeys $ ctf t
        getKeys = S.unions . concatMap keysFromEnt
    keyAttrs = filter attrIdent
    attr2vtx' en attr = Vertex en $ attrName attr
    attrs2vtx' en = S.fromList . map (attr2vtx' en)
    attrs2vtx = attrs2vtx' . entName
    (-->) :: VertexList -> VertexList -> Edge
    (-->) a b = (a, b)
