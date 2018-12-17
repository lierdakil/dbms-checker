{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Algo.RSTools.Types where

import Data.HashSet as S
import Data.Text.Lazy (Text)
import Algo.FDTools.Types
import Data.Hashable
import GHC.Generics

data NumberClass = Natural | Whole | Rational
    deriving (Eq, Generic, Hashable, Ord)
data Domain = DomainNumber NumberClass
            | DomainString
            | DomainText
            | DomainDateTime
            | DomainDate
            | DomainTime
            | DomainEnum (S.HashSet Text)
            | DomainOther Text
    deriving (Eq, Generic, Hashable, Ord)
data Attribute = Attribute {
      attributeIsKey :: Bool
    , attributeName :: Vertex
    , attributeDomain :: Domain
    } deriving (Eq, Generic, Hashable)
newtype Relation = Relation { relationAttributes :: S.HashSet Attribute }
    deriving (Eq, Generic, Hashable)
newtype Relations = Relations { relationsSet :: S.HashSet Relation }
