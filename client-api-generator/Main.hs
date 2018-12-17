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
import Data.UUID
import Data.Time

instance TypeScript UUID where
  getTypeScriptDeclarations _ = []
  getTypeScriptType _ = "string"

instance TypeScript UTCTime where
  getTypeScriptDeclarations _ = []
  getTypeScriptType _ = "string"

$(mconcat <$> traverse (deriveTypeScript jsonDerivationOptions) allJsonTypes)

main :: IO ()
main =
  putStrLn $ formatTSDeclarations $
    concat $(foldr (\x acc -> [|getTypeScriptDeclarations $(makeProxy x) : $acc|]) [|[]|] allJsonTypes)
