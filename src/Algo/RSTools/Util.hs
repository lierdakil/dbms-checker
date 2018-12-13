module Algo.RSTools.Util where

import Algo.FDTools.Types
import Algo.FDTools.Util
import Algo.RSTools.Types
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

fdsFromRelSchema :: Relations -> Graph
fdsFromRelSchema (Relations rels) = M.fromListWith (<>)
  $ map relationFDs
  $ S.toList rels
  where
    relationFDs (Relation attr) = (S.fromList $ map attributeName lhsa
                                 , S.fromList $ map attributeName rhsa)
      where (lhsa, rhsa) = partition attributeIsKey $ S.toList attr

relProject :: Relation -> Graph -> Graph
relProject (Relation attrs) allFDs = project (S.map attributeName attrs) allFDs

allRelationFDs :: Graph -> Relations -> S.HashSet (Relation, Graph)
allRelationFDs allFDs (Relations rels) = S.map (\rel -> (rel, relProject rel allFDs)) rels

relFDsNotInNFEK :: Graph -> (Relation, Graph) -> Graph
relFDsNotInNFEK efds (r, g) = nfek nfbc' ek
  where
    nfbc' = nfbc g sk
    sk = superkeys g
    ek = elementarykeys $ relProject r efds

allAttrMap :: Relations -> M.HashMap Vertex (NonEmpty (Relation, Attribute))
allAttrMap (Relations rels) = M.fromListWith (<>) $ concatMap relationToAttrMap $ S.toList rels

relationToAttrMap :: Relation -> [(Vertex, NE.NonEmpty (Relation, Attribute))]
relationToAttrMap r@(Relation attrs) = map (\a -> (attributeName a, (r, a) :| [])) $ S.toList attrs

isSuperKey :: VertexList -> Graph -> Relation -> Bool
isSuperKey vs g (Relation attrs) = rvs == closure vs (project rvs g)
  where rvs = S.map attributeName attrs
