{-# LANGUAGE DuplicateRecordFields, TypeFamilies, DataKinds, RecordWildCards #-}
module API.Types where

import Data.Text (Text)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import DB.Types

data UserSessionData = UserSessionData {
    userSessionUserId :: UserIdentifier
  , userSessionUserName :: T.Text
  , userSessionUserRole :: Role
}

data AuthData = AuthData {
    authLogin :: T.Text
  , authPassword :: T.Text
  }

data AssignedTopicInfo =
    AssignedTopicInfoPredefined PredefinedTopic
  | AssignedTopicInfoCustom CustomTopic

newtype FileData = FileData { unFileData :: BL.ByteString }

type family CanValidate (idType :: *) where
  CanValidate FunDepIdentifier = 'True
  CanValidate RelSchemaIdentifier = 'True
  CanValidate PhysSchemaIdentifier = 'True
  CanValidate idType = 'False

type family CanAccept (idType :: *) where
  CanAccept ERDIdentifier = 'True
  CanAccept PhysSchemaIdentifier = 'True
  CanAccept idType = 'False

type BasicCrudResponseBody (idType :: *) =
    BasicCrudResponseBodyInternal (CanValidate idType) (CanAccept idType) idType

type family BasicCrudResponseBodyInternal (canValidate :: Bool) (canAccept :: Bool) :: (* -> *) where
    BasicCrudResponseBodyInternal 'False 'False = BasicCrudResponseBodyWithoutAnything
    BasicCrudResponseBodyInternal 'True 'False = BasicCrudResponseBodyWithValidation
    BasicCrudResponseBodyInternal 'False 'True = BasicCrudResponseBodyWithAcceptance
    BasicCrudResponseBodyInternal 'True 'True = BasicCrudResponseBodyWithAcceptanceAndValidation

data BasicCrudResponseBodyWithoutAnything idType = BasicCrudResponseBodyWithoutAnything {
    id :: !idType
  , description :: !Text
  }

data BasicCrudResponseBodyWithValidation idType = BasicCrudResponseBodyWithValidation {
    id :: !idType
  , description :: !Text
  , validationErrors :: ![Text]
  }

data BasicCrudResponseBodyWithAcceptance idType = BasicCrudResponseBodyWithAcceptance {
    id :: !idType
  , description :: !Text
  , accepted :: !AcceptanceState
  }

data BasicCrudResponseBodyWithAcceptanceAndValidation idType = BasicCrudResponseBodyWithAcceptanceAndValidation {
    id :: !idType
  , description :: !Text
  , validationErrors :: ![Text]
  , accepted :: !AcceptanceState
  }

type family Identifier a
type instance Identifier ERDiagram = ERDIdentifier
type instance Identifier FunctionalDependencies = FunDepIdentifier
type instance Identifier RelationalSchema = RelSchemaIdentifier
type instance Identifier PhysicalSchema = PhysSchemaIdentifier

class HasResponseBody a where
  toResponseBody :: a -> BasicCrudResponseBody (Identifier a)

instance HasResponseBody ERDiagram where
  toResponseBody ERDiagram{..} = BasicCrudResponseBodyWithAcceptance {
      id = id
    , description = diagram
    , accepted = accepted
    }
instance HasResponseBody FunctionalDependencies where
  toResponseBody FunctionalDependencies{..} = BasicCrudResponseBodyWithValidation {
      id = id
    , description = funDeps
    , validationErrors = []
    }
instance HasResponseBody RelationalSchema where
  toResponseBody RelationalSchema{..} = BasicCrudResponseBodyWithValidation {
      id = id
    , description = relations
    , validationErrors = []
    }
instance HasResponseBody PhysicalSchema where
  toResponseBody PhysicalSchema{..} = BasicCrudResponseBodyWithAcceptanceAndValidation {
      id = id
    , description = schemaSQL
    , accepted = accepted
    , validationErrors = []
    }

data UserInfo = UserInfo {
    userInfoUserId :: UserIdentifier
  , userInfoUsername :: Text
  , userInfoUserRole :: Role
  , userInfoUserGroup :: Group
  }

data CommentInfo = CommentInfo {
    id :: CommentIdentifier
  , parentItem :: ParentItemIdentifier
  , parentComment :: ParentComment
  , commentAuthor :: UserInfo
  , commentPrio :: CommentPriority
  , commentText :: Text
  , commentStatus :: CommentStatus
  }

data CommentBodyInfo = CommentBodyInfo {
    parentItem :: ParentItemIdentifier
  , parentComment :: ParentComment
  , commentPrio :: CommentPriority
  , commentText :: Text
  }

data CommentStatusInfo = CommentStatusInfo {
    commentStatus :: CommentStatus
  }

type ERDBody = BasicCrudResponseBodyWithAcceptance ERDIdentifier
type FunDepBody = BasicCrudResponseBodyWithValidation FunDepIdentifier
type RelSchemaBody = BasicCrudResponseBodyWithValidation RelSchemaIdentifier
type PhysSchemaBody = BasicCrudResponseBodyWithAcceptanceAndValidation PhysSchemaIdentifier
