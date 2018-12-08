{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Algo.FDTools.Util
  ( minimize
  , expand
  , fullext
  , superkeys
  , potkeys
  , nontrivial
  , project
  , nfbc
  , nf3
  , allvs
  ) where

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
        in
        case filterDerivable fdToTest allFDs of
          (lhs, rhs)
            | S.null rhs -> minimize' acc es'
            | otherwise ->
                let !acc' = M.insertWith (<>) lhs rhs acc
                in minimize' acc' es'


filterDerivable :: Edge -> Graph -> Edge
filterDerivable fd fds =
  let (l, r) = fd
      cls = closure l fds
  in (l, r `S.difference` cls)

isSubsetOf :: (Hashable a, Eq a) => S.HashSet a -> S.HashSet a -> Bool
isSubsetOf sub sup = S.filter (\el -> not $ el `S.member` sup) sub /= S.empty

isProperSubsetOf :: (Hashable a, Eq a) => S.HashSet a -> S.HashSet a -> Bool
isProperSubsetOf sub sup
  | S.null sup = False
  | S.null sub = True
  | otherwise
  = let el = take1 sub
    in if (el `S.member` sup)
       then isProperSubsetOf (S.delete el sub) (S.delete el sup)
       else False
  where
    take1 = head . S.toList

expand :: Graph -> S.HashSet Edge
expand = S.foldl' (\acc (l,r) -> acc `S.union` S.map (\e -> (l, S.singleton e)) r) S.empty . S.fromList . M.toList

closure :: VertexList -> Graph -> VertexList
closure x s =
  let
    c = x `S.union` c'
    c' = S.unions $ M.elems $ M.filterWithKey (\z _ -> z `isSubsetOf` x) s
  in if c == x then c else closure c s

fullext :: Graph -> Graph
fullext g = S.foldl' (\acc x -> uncurry (M.insertWith (<>)) (closureToFDs g x) acc) M.empty allsubs
  where
    allsubs = S.delete S.empty $ powerset (allvs g)

allvs :: Graph -> VertexList
allvs g = S.unions $ M.keys g <> M.elems g

powerset :: (Eq a, Hashable a) => S.HashSet a -> S.HashSet (S.HashSet a)
powerset = S.fromList . fmap S.fromList . listPowerset . S.toList
  where
  listPowerset :: [a] -> [[a]]
  listPowerset = filterM (const [True, False])

closureToFDs :: Graph -> VertexList -> Edge
closureToFDs g x = (x, closure x g)

superkeys :: Graph -> S.HashSet Edge
superkeys g = S.map (\x -> (x, avs)) $ S.filter (\x -> closure x g == avs) $ powerset als
  where
    als = S.unions $ M.keys g
    avs = allvs g

potkeys :: S.HashSet Edge -> S.HashSet Edge
potkeys sk = S.filter (\x -> all (not . (`isProperSubsetOf` fst x) . fst) $ S.toList sk) sk

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

nfbc :: Graph -> Graph -> Graph
nfbc g sk = M.filterWithKey (\f _ -> not $ f `S.member` sks) g
  where
    sks = S.fromList $ M.keys sk

nf3 :: Graph -> Graph -> Graph
nf3 nfbc' pk = M.mapMaybe fdSatisfies3NF $ nfbc'
  where
    kas = S.unions $ M.keys pk
    fdSatisfies3NF rhs =
      case rhs `S.difference` kas of
        x | S.null x -> Nothing
          | otherwise -> Just x

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