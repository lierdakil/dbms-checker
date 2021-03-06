{-# LANGUAGE TemplateHaskellQuotes #-}
module API.TypeLists where

import API.Types
import DB.Types
import Language.Haskell.TH.Syntax (Name)

allJsonTypes, jsonOnlyTypes, jsonAndSchemaTypes :: [Name]

allJsonTypes = jsonAndSchemaTypes <> jsonOnlyTypes

jsonOnlyTypes = [
    ''ParentComment
  , ''BasicCrudResponseBodyWithoutAnything
  , ''BasicCrudResponseBodyWithValidation
  , ''BasicCrudResponseBodyWithAcceptance
  , ''BasicCrudResponseBodyWithAcceptanceAndValidation
  , ''Group
  ]

jsonAndSchemaTypes = [
    ''UserIdentifier
  , ''UserInfo
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
  , ''CommentPriority
  , ''CommentStatus
  ]
