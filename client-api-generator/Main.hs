{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, DeriveGeneric
    , StandaloneDeriving, TemplateHaskell, ScopedTypeVariables, KindSignatures
    , TypeOperators
    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import API.Types
import API.JsonDeriv
import DBTypes
import Data.Proxy
import Data.Aeson.TypeScript.TH

instance TypeScript Word where
  getTypeScriptDeclarations _ = []
  getTypeScriptType _ = "number"

$(mconcat <$> traverse (deriveTypeScript jsonDerivationOptions) [
      ''UserIdentifier
    , ''PredefinedTopicIdentifier
    , ''CustomTopicIdentifier
    , ''ERDIdentifier
    , ''CommentIdentifier
    , ''FunDepIdentifier
    , ''RelSchemaIdentifier
    , ''PhysSchemaIdentifier
    , ''Role
    , ''AuthData
    , ''UserSessionData
    , ''PredefinedTopic
    , ''AssignedTopicInfo
    , ''CustomTopic
    , ''AcceptanceState
    , ''AssignedTopic
    , ''CommentInfo
    , ''CommentBodyInfo
    , ''ParentItemIdentifier
    , ''ParentComment
    , ''CommentPriority
    , ''CommentStatus
    , ''CommentStatusInfo
    , ''BasicCrudResponseBodyWithoutValidation
    , ''BasicCrudResponseBodyWithValidation
    ])

main :: IO ()
main = do
  putStrLn $ formatTSDeclarations $
    concat [
      getTypeScriptDeclarations (Proxy :: Proxy (UserIdentifier))
    , getTypeScriptDeclarations (Proxy :: Proxy (PredefinedTopicIdentifier))
    , getTypeScriptDeclarations (Proxy :: Proxy (CustomTopicIdentifier))
    , getTypeScriptDeclarations (Proxy :: Proxy (ERDIdentifier))
    , getTypeScriptDeclarations (Proxy :: Proxy (CommentIdentifier))
    , getTypeScriptDeclarations (Proxy :: Proxy (FunDepIdentifier))
    , getTypeScriptDeclarations (Proxy :: Proxy (RelSchemaIdentifier))
    , getTypeScriptDeclarations (Proxy :: Proxy (PhysSchemaIdentifier))
    , getTypeScriptDeclarations (Proxy :: Proxy (Role))
    , getTypeScriptDeclarations (Proxy :: Proxy (AuthData))
    , getTypeScriptDeclarations (Proxy :: Proxy (UserSessionData))
    , getTypeScriptDeclarations (Proxy :: Proxy (PredefinedTopic))
    , getTypeScriptDeclarations (Proxy :: Proxy (AssignedTopicInfo))
    , getTypeScriptDeclarations (Proxy :: Proxy (CustomTopic))
    , getTypeScriptDeclarations (Proxy :: Proxy (AcceptanceState))
    , getTypeScriptDeclarations (Proxy :: Proxy (AssignedTopic))
    , getTypeScriptDeclarations (Proxy :: Proxy (CommentInfo))
    , getTypeScriptDeclarations (Proxy :: Proxy (CommentBodyInfo))
    , getTypeScriptDeclarations (Proxy :: Proxy (ParentItemIdentifier))
    , getTypeScriptDeclarations (Proxy :: Proxy (ParentComment))
    , getTypeScriptDeclarations (Proxy :: Proxy (CommentPriority))
    , getTypeScriptDeclarations (Proxy :: Proxy (CommentStatus))
    , getTypeScriptDeclarations (Proxy :: Proxy (CommentStatusInfo))
    , getTypeScriptDeclarations (Proxy :: Proxy (BasicCrudResponseBodyWithoutValidation))
    , getTypeScriptDeclarations (Proxy :: Proxy (BasicCrudResponseBodyWithValidation))
    ]
