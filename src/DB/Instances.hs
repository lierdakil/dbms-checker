{-# LANGUAGE StandaloneDeriving, TemplateHaskell, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module DB.Instances () where

import ProjectM36.Base
import ProjectM36.Atomable

import DB.TH
import DB.TypeLists
import DB.Types
import Data.UUID
import Data.Maybe
import Data.Hashable

instance Atomable UUID where
  toAtom = ByteStringAtom . toASCIIBytes
  fromAtom (ByteStringAtom i) = fromMaybe (error "couldn't parse UUID") $ fromASCIIBytes i
  fromAtom e = error ("improper fromAtom" ++ show e)
  toAtomType _ = ByteStringAtomType
  toAddTypeExpr _ = NoOperation

instance Hashable CommentIdentifier
instance Hashable ParentComment

$(mconcat <$> traverse deriveGeneric (domains <> DB.TypeLists.relations <> joins))
$(mconcat <$> traverse deriveAtomable (''User : domains))
$(mconcat <$> traverse deriveTupleable (DB.TypeLists.relations <> joins))
