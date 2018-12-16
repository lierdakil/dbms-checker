{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Algo.FDTools.Util where

import Algo.FDTools.Types
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Control.Monad
import Data.Hashable
import Data.Function
import Data.List

minimize :: Graph -> Graph
minimize = minimize' M.empty . nontrivial
  where
    minimize' :: Graph -> Graph -> Graph
    minimize' acc es
      | M.null es = acc
      | otherwise =
        let
          -- find element with longest left side
          fdToTest = maximumBy compareHeadLen $ M.toList es
          compareHeadLen = compare `on` (S.size . fst)
          !es' = M.delete (fst fdToTest) es
          !allFDs = M.unionWith (<>) es' acc
          handleRest (lhs, rhs) = M.insertWith (<>) lhs rhs acc
        in
        flip minimize' es' $ maybe acc handleRest $ filterDerivable fdToTest allFDs

getUnderiveable :: Graph -> Graph -> Graph
getUnderiveable fds from = M.mapMaybeWithKey f fds
  where f lhs rhs = snd <$> filterDerivable (lhs, rhs) from

filterDerivable :: Edge -> Graph -> Maybe Edge
filterDerivable (l, r) fds =
  let diff = r `S.difference` closure l fds
  in if S.null diff
     then Nothing
     else Just (l, diff)

isSubsetOf :: (Hashable a, Eq a) => S.HashSet a -> S.HashSet a -> Bool
isSubsetOf sub sup = S.foldl' (\acc x -> acc && x `S.member` sup) True sub

isProperSubsetOf :: (Hashable a, Eq a) => S.HashSet a -> S.HashSet a -> Bool
isProperSubsetOf sub sup = S.size sub < S.size sup && isSubsetOf sub sup

expand :: Graph -> [(VertexList, Vertex)]
expand = concatMap (\(l,r) -> map (\x -> (l, x)) $ S.toList r) . M.toList

closure :: VertexList -> Graph -> VertexList
closure x s =
  let c = x `S.union` (
        S.unions $ M.elems $ M.filterWithKey (\z _ -> z `isSubsetOf` x) s
        )
  in if c == x then c else closure c s

fullext :: Graph -> Graph
fullext g = S.foldl' (\acc x -> uncurry (M.insertWith (<>)) (closureToFDs g x) acc) M.empty allsubs
  where
    allsubs = S.delete S.empty $ powerset (allvs g)

allvs :: Graph -> VertexList
allvs g = S.unions $ M.keys g <> M.elems g

powerset :: (Eq a, Hashable a) => S.HashSet a -> S.HashSet (S.HashSet a)
powerset = S.fromList . fmap S.fromList . listPowerset . S.toList

listPowerset :: [a] -> [[a]]
listPowerset = filterM (const [True, False])

closureToFDs :: Graph -> VertexList -> Edge
closureToFDs g x = (x, closure x g)

superkeys :: Graph -> S.HashSet VertexList
superkeys g = S.filter (\x -> closure x g == avs) $ powerset als
  where
    als = S.unions $ M.keys g
    avs = allvs g

potkeys :: S.HashSet VertexList -> S.HashSet VertexList
potkeys sk = S.filter (\x -> all (not . (`isProperSubsetOf` x)) $ S.toList sk) sk

elementarykeys :: Graph -> S.HashSet VertexList
elementarykeys efds = S.fromList $ M.keys efds

elementaryFDs :: Graph -> Graph
elementaryFDs g = M.fromListWith (<>) $ concatMap (uncurry elementaryFDsFromFD) $ M.toList g
  where
    elementaryFDsFromFD lhs rhs = elementaryFDsFromFD' lhs $ S.toList rhs
    elementaryFDsFromFD' lhs rhs = do
      l <- S.toList lhs
      let reducedLhs = S.delete l lhs
          cls = closure reducedLhs g
          (deriv, notDeriv) = partition (`S.member` cls) rhs
          rest = elementaryFDsFromFD' reducedLhs deriv
      if null notDeriv
      then rest
      else (reducedLhs, S.fromList notDeriv):rest

nontrivial :: Graph -> Graph
nontrivial = M.mapMaybeWithKey removeTrivialFromLeftSide
  where
    removeTrivialFromLeftSide rhs lhs =
      case lhs `S.difference` rhs of
        x | S.null x -> Nothing
          | otherwise -> Just x

project :: VertexList -> Graph -> Graph
project p = M.map (S.intersection p)
          . M.filterWithKey (\f _ -> f `isSubsetOf` p)

nfbc :: Graph -> S.HashSet VertexList -> Graph
nfbc g sks = M.filterWithKey (\f _ -> not $ f `S.member` sks) g

nfek :: Graph -> S.HashSet VertexList -> Graph
nfek nfbc' ek = M.filter fdSatisfiesNFEK $ nfbc'
  where
    ekas = S.foldr (S.union) S.empty ek
    fdSatisfiesNFEK rhs = not $ S.null $ rhs `S.difference` ekas

nf3 :: Graph -> S.HashSet VertexList -> Graph
nf3 nfbc' pk = M.filter fdSatisfies3NF $ nfbc'
  where
    kas = S.foldr (S.union) S.empty pk
    fdSatisfies3NF rhs = not $ S.null $ rhs `S.difference` kas

-- normalize :: Graph -> Graph -> Either [[Vertex]] [[Vertex]]
-- normalize g invFD = checkErr
--   where
--     -- restFD = S.difference g invFD
--     invAttrR = unions $ S.map snd invFD
--     -- restAttrL = unions $ S.map fst restFD
--     invRels :: [[Vertex]]
--     invRels = S.toList $ S.map (S.toList . uncurry S.union) invFD
--     nf1 = unions $ S.map (uncurry S.union) g
--     baseRel :: [Vertex]
--     baseRel = S.toList $ nf1 `S.difference` invAttrR
--     -- basePrj = conservative nontrivial $ project (S.fromList baseRel) $ fullext g
--     result | S.null $ g `S.difference` invFD = invRels
--            | otherwise =  baseRel : invRels
--     -- prjFDs = S.unions $ map (\x -> project (S.fromList x) $ fullext g) result
--     checkErr = Right result
--     -- checkErr = if fullext prjFDs == fullext g
--     --            then Right result
--     --            else Left result
