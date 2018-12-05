{-# LANGUAGE DuplicateRecordFields #-}
module DBTypes where

import Servant (FromHttpApiData(..))
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Time (UTCTime)

import Data.Word (Word)

-- Domains

newtype UserIdentifier = UserIdentifier Word

newtype PredefinedTopicIdentifier = PredefinedTopicIdentifier Word

newtype CustomTopicIdentifier = CustomTopicIdentifier Word

newtype ERDIdentifier = ERDIdentifier Word

newtype CommentIdentifier = CommentIdentifier Word

newtype FunDepIdentifier = FunDepIdentifier Word

newtype RelSchemaIdentifier = RelSchemaIdentifier Word

newtype PhysSchemaIdentifier = PhysSchemaIdentifier Word

data Role = Student | Teacher

data Group = NoGroup | Group Text

data AcceptanceState = Accepted | NotAccepted

data AssignedTopic =
        PredefinedAssignedTopic PredefinedTopicIdentifier
      | CustomAssignedTopic CustomTopicIdentifier

data ParentItemIdentifier =
        ParentTopicSelection UserIdentifier
      | ParentERD ERDIdentifier
      | ParentFunDep FunDepIdentifier
      | ParentRelSchema RelSchemaIdentifier
      | ParentPhysSchema PhysSchemaIdentifier

data ParentComment =
        NoParentComment
      | ParentComment CommentIdentifier

data CommentPriority =
        CommentStatusNormal
      | CommentStatusImportant
      | CommentStatusCritical

data CommentStatus =
        CommentStateOpen
      | CommentStateClosed

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
  }

data CustomTopic = CustomTopic {
    id :: CustomTopicIdentifier
  , name :: Text
  , accepted :: AcceptanceState
  }

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
