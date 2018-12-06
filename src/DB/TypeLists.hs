{-# LANGUAGE TemplateHaskellQuotes #-}

module DB.TypeLists where

import DB.Types
import Language.Haskell.TH.Syntax (Name)

domains :: [Name]
domains = [
    ''UserIdentifier
  , ''PredefinedTopicIdentifier
  , ''CustomTopicIdentifier
  , ''ERDIdentifier
  , ''CommentIdentifier
  , ''FunDepIdentifier
  , ''RelSchemaIdentifier
  , ''PhysSchemaIdentifier
  , ''Role
  , ''Group
  , ''AcceptanceState
  , ''AssignedTopic
  , ''ParentItemIdentifier
  , ''ParentComment
  , ''CommentPriority
  , ''CommentStatus
  ]

relations :: [Name]
relations = [
    ''User
  , ''PredefinedTopic
  , ''CustomTopic
  , ''TopicAssignment
  , ''ERDiagram
  , ''Comment
  , ''FunctionalDependencies
  , ''RelationalSchema
  , ''PhysicalSchema
  ]
