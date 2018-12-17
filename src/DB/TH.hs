{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
module DB.TH where

import Language.Haskell.TH.Syntax
import Control.Monad
import Control.DeepSeq
import ProjectM36.Atomable
import ProjectM36.Tupleable
import GHC.Generics
import Data.Binary

deriveGeneric :: Name -> Q [Dec]
deriveGeneric t =
  let typ = return $ ConT t
  in [d|deriving instance Generic $typ|]

deriveAtomable :: Name -> Q [Dec]
deriveAtomable t =
  let typ = return $ ConT t
      (<<>>) = liftM2 (<>)
  in
    [d|deriving instance Show $typ|]
    <<>> [d|deriving instance Eq $typ|]
    <<>> [d|instance NFData $typ|]
    <<>> [d|instance Binary $typ|]
    <<>> [d|instance Atomable $typ|]

deriveTupleable :: Name -> Q [Dec]
deriveTupleable t =
  let typ = return $ ConT t
  in [d|instance Tupleable $typ|]
