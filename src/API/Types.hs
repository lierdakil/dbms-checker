{-# LANGUAGE DuplicateRecordFields, TypeFamilies, DataKinds #-}
module API.Types where

import Data.Text (Text)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import DBTypes

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
    id :: idType
  , description :: Text
  }

data BasicCrudResponseBodyWithValidation idType = BasicCrudResponseBodyWithValidation {
    id :: idType
  , description :: Text
  , validationErrors :: [Text]
  }

data BasicCrudResponseBodyWithAcceptance idType = BasicCrudResponseBodyWithAcceptance {
    id :: idType
  , description :: Text
  , accepted :: AcceptanceState
  }

data BasicCrudResponseBodyWithAcceptanceAndValidation idType = BasicCrudResponseBodyWithAcceptanceAndValidation {
    id :: idType
  , description :: Text
  , validationErrors :: [Text]
  , accepted :: AcceptanceState
  }

data CommentInfo = CommentInfo {
    id :: CommentIdentifier
  , parentItem :: ParentItemIdentifier
  , parentComment :: ParentComment
  , commentAuthor :: Text -- username
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
