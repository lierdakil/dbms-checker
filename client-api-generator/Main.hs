{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, DeriveGeneric
    , StandaloneDeriving, TemplateHaskell, ScopedTypeVariables, KindSignatures
    , TypeOperators
    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import API.JsonDeriv
import API.TypeLists
import API.TH
import Data.Aeson.TypeScript.TH

instance TypeScript Word where
  getTypeScriptDeclarations _ = []
  getTypeScriptType _ = "number"

$(mconcat <$> traverse (deriveTypeScript jsonDerivationOptions) allJsonTypes)

main :: IO ()
main = do
  putStrLn $ formatTSDeclarations $
    concat $(foldr (\x acc -> [|getTypeScriptDeclarations $(makeProxy x) : $acc|]) [|[]|] allJsonTypes)
