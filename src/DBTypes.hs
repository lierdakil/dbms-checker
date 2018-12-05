{-# LANGUAGE DuplicateRecordFields, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, UndecidableInstances #-}
module DBTypes where

import Servant (FromHttpApiData(..))
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Time (UTCTime)

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema(..), ToParamSchema(..))
import Data.Swagger.Schema (genericDeclareNamedSchemaUnrestricted, defaultSchemaOptions)
import Data.Word (Word)

-- Domains

newtype UserIdentifier = UserIdentifier Word
  deriving (Generic)
  deriving anyclass (ToSchema, ToParamSchema)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData)

newtype PredefinedTopicIdentifier = PredefinedTopicIdentifier Word
  deriving (Generic)
  deriving anyclass (ToSchema, ToParamSchema)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData)

newtype CustomTopicIdentifier = CustomTopicIdentifier Word
  deriving (Generic)
  deriving anyclass (ToSchema, ToParamSchema)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData)

newtype ERDIdentifier = ERDIdentifier Word
  deriving (Generic)
  deriving anyclass (ToSchema, ToParamSchema)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData)

newtype CommentIdentifier = CommentIdentifier Word
  deriving (Generic)
  deriving anyclass (ToSchema, ToParamSchema)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData)

newtype FunDepIdentifier = FunDepIdentifier Word
  deriving (Generic)
  deriving anyclass (ToSchema, ToParamSchema)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData)

newtype RelSchemaIdentifier = RelSchemaIdentifier Word
  deriving (Generic)
  deriving anyclass (ToSchema, ToParamSchema)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData)

newtype PhysSchemaIdentifier = PhysSchemaIdentifier Word
  deriving (Generic)
  deriving anyclass (ToSchema, ToParamSchema)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData)

data Role = Student | Teacher
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data Group = NoGroup | Group Text

data AcceptanceState = Accepted | NotAccepted
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data AssignedTopic =
        PredefinedAssignedTopic PredefinedTopicIdentifier
      | CustomAssignedTopic CustomTopicIdentifier
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data ParentItemIdentifier =
        ParentTopicSelection UserIdentifier
      | ParentERD ERDIdentifier
      | ParentFunDep FunDepIdentifier
      | ParentRelSchema RelSchemaIdentifier
      | ParentPhysSchema PhysSchemaIdentifier
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data ParentComment =
        NoParentComment
      | ParentComment CommentIdentifier
  deriving (Generic, FromJSON, ToJSON)

instance ToSchema ParentComment where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

data CommentPriority =
        CommentStatusNormal
      | CommentStatusImportant
      | CommentStatusCritical
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data CommentStatus =
        CommentStateOpen
      | CommentStateClosed
  deriving (Generic, FromJSON, ToJSON, ToSchema)

-- Relations

data User = User {
    id :: UserIdentifier
  , username :: Text
  , group :: Group
  , saltedPasswordHash :: ByteString
  , passwordSalt :: ByteString
  , registrationDate :: UTCTime
  , role :: Role
  }

data PredefinedTopic = PredefinedTopic {
    id :: PredefinedTopicIdentifier
  , name :: Text
  } deriving (Generic, FromJSON, ToJSON, ToSchema)

data CustomTopic = CustomTopic {
    id :: CustomTopicIdentifier
  , name :: Text
  , accepted :: AcceptanceState
  } deriving (Generic, FromJSON, ToJSON, ToSchema)

data TopicAssignments = TopicAssignments {
    userId :: UserIdentifier
  , topic :: AssignedTopic
}

data ERDiagram = ERDiagram {
    id :: ERDIdentifier
  , userId :: UserIdentifier
  , diagram :: Text
  , accepted :: AcceptanceState
  }

data Comment = Comment {
    id :: CommentIdentifier
  , parentItem :: ParentItemIdentifier
  , parentComment :: ParentComment
  , commentAuthor :: UserIdentifier
  , commentPrio :: CommentPriority
  , commentText :: Text
  , commentStatus :: CommentStatus
  }

data FunctionalDependencies = FunctionalDependencies {
    id :: FunDepIdentifier
  , userId :: UserIdentifier
  , funDeps :: Text
  }

data RelationalSchema = RelationalSchema {
    id :: RelSchemaIdentifier
  , userId :: UserIdentifier
  , relations :: Text
  }

data PhysicalSchema = PhysicalSchema {
    id :: PhysSchemaIdentifier
  , userId :: UserIdentifier
  , schemaSQL :: Text
  , accepted :: AcceptanceState
  }
