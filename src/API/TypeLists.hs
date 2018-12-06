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
  ]

jsonAndSchemaTypes = [
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
  , ''CommentPriority
  , ''CommentStatus
  , ''CommentStatusInfo
  ]
