{-# LANGUAGE DuplicateRecordFields, DeriveAnyClass #-}
module DB.Types where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Time (UTCTime)

import Data.UUID (UUID)

-- Domains

newtype UserIdentifier = UserIdentifier UUID
newtype PredefinedTopicIdentifier = PredefinedTopicIdentifier UUID
newtype CustomTopicIdentifier = CustomTopicIdentifier UUID
newtype ERDIdentifier = ERDIdentifier UUID
newtype CommentIdentifier = CommentIdentifier UUID
newtype FunDepIdentifier = FunDepIdentifier UUID
newtype RelSchemaIdentifier = RelSchemaIdentifier UUID
newtype PhysSchemaIdentifier = PhysSchemaIdentifier UUID

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
    id :: !UserIdentifier
  , username :: !Text
  , group :: !Group
  , email :: !Text
  , saltedPasswordHash :: !ByteString
  , registrationDate :: !UTCTime
  , role :: !Role
  }

data PredefinedTopic = PredefinedTopic {
    id :: !PredefinedTopicIdentifier
  , name :: !Text
  }

data CustomTopic = CustomTopic {
    id :: !CustomTopicIdentifier
  , name :: !Text
  , topicAuthor :: !UserIdentifier
  , accepted :: !AcceptanceState
  }

data TopicAssignment = TopicAssignment {
    userId :: !UserIdentifier
  , topic :: !AssignedTopic
  }

data ERDiagram = ERDiagram {
    id :: !ERDIdentifier
  , userId :: !UserIdentifier
  , diagram :: !Text
  , derivedFDs :: !(Maybe ByteString)
  , accepted :: !AcceptanceState
  , validationErrors :: ![Text]
  }

data Comment = Comment {
    id :: !CommentIdentifier
  , commentTime :: !UTCTime
  , parentItem :: !ParentItemIdentifier
  , parentComment :: !ParentComment
  , commentAuthor :: !UserIdentifier
  , commentPrio :: !CommentPriority
  , commentText :: !Text
  , commentStatus :: !CommentStatus
  }

data FunctionalDependencies = FunctionalDependencies {
    id :: !FunDepIdentifier
  , userId :: !UserIdentifier
  , funDeps :: !Text
  , validationErrors :: ![Text]
  }

data RelationalSchema = RelationalSchema {
    id :: !RelSchemaIdentifier
  , userId :: !UserIdentifier
  , relations :: !Text
  , validationErrors :: ![Text]
  }

data PhysicalSchema = PhysicalSchema {
    id :: !PhysSchemaIdentifier
  , userId :: !UserIdentifier
  , schemaSQL :: !Text
  , accepted :: !AcceptanceState
  , validationErrors :: ![Text]
  }

-- Helper join types

data CommentWithUserInfo = CommentWithUserInfo {
    id :: !CommentIdentifier
  , commentTime :: !UTCTime
  , parentItem :: !ParentItemIdentifier
  , parentComment :: !ParentComment
  , commentAuthor :: !UserIdentifier
  , commentPrio :: !CommentPriority
  , commentText :: !Text
  , commentStatus :: !CommentStatus
  , authorUsername :: !Text
  , authorGroup :: !Group
  , authorEmail :: !Text
  , authorRegistrationDate :: !UTCTime
  , authorRole :: !Role
  }
