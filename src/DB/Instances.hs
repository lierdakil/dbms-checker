{-# LANGUAGE StandaloneDeriving, TemplateHaskell, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module DB.Instances () where

import ProjectM36.Base
import ProjectM36.Atomable

import DB.TH
import DB.TypeLists
import DB.Types

instance Atomable Word where
  toAtom = IntegerAtom . fromIntegral
  fromAtom (IntegerAtom i) = fromIntegral i
  fromAtom e = error ("improper fromAtom" ++ show e)
  toAtomType _ = IntegerAtomType
  toAddTypeExpr _ = NoOperation

$(mconcat <$> traverse deriveAtomable domains)

$(mconcat <$> traverse deriveTupleable DB.TypeLists.relations)
