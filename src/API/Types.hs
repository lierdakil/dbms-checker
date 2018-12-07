{-# LANGUAGE DuplicateRecordFields, TypeFamilies, DataKinds, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, NamedFieldPuns #-}
module API.Types where

import Data.Text (Text)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List (foldl')

import DB.Types
import DB.Instances ()

data UserSessionData = UserSessionData {
    userSessionUserInfo :: !UserInfo
  , userSessionKey :: !String
}

userSessionUserId :: UserSessionData -> UserIdentifier
userSessionUserId = userInfoUserId . userSessionUserInfo

userSessionUserRole :: UserSessionData -> Role
userSessionUserRole = userInfoUserRole . userSessionUserInfo

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
  type family ResponseBody a
  type ResponseBody a = BasicCrudResponseBody (Identifier a)
  toResponseBody :: a -> ResponseBody a

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
    , validationErrors = validationErrors
    }
instance HasResponseBody RelationalSchema where
  toResponseBody RelationalSchema{..} = BasicCrudResponseBodyWithValidation {
      id = id
    , description = relations
    , validationErrors = validationErrors
    }
instance HasResponseBody PhysicalSchema where
  toResponseBody PhysicalSchema{..} = BasicCrudResponseBodyWithAcceptanceAndValidation {
      id = id
    , description = schemaSQL
    , accepted = accepted
    , validationErrors = validationErrors
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
  , childrenComments :: [CommentInfo]
  , commentAuthor :: UserInfo
  , commentPrio :: CommentPriority
  , commentText :: Text
  , commentStatus :: CommentStatus
  }

instance HasResponseBody User where
  type instance ResponseBody User = UserInfo
  toResponseBody User{..} = UserInfo {
      userInfoUserId = id
    , userInfoUserRole = role
    , userInfoUsername = username
    , userInfoUserGroup = group
    }

instance HasResponseBody [CommentWithUserInfo] where
  type instance ResponseBody [CommentWithUserInfo] = [CommentInfo]
  toResponseBody comments = buildForest NoParentComment
    where
      parentMap :: M.Map ParentComment [CommentWithUserInfo]
      parentMap = foldl' foldf M.empty comments
      foldf acc x@(CommentWithUserInfo{parentComment}) = M.insertWith (<>) parentComment [x] acc
      buildForest parent = maybe [] (map toCommentInfo) $ M.lookup parent parentMap
      toCommentInfo x@CommentWithUserInfo{id=id'} =
        (toResponseBody x){childrenComments = buildForest $ ParentComment id'}

instance HasResponseBody CommentWithUserInfo where
  type instance ResponseBody CommentWithUserInfo = CommentInfo
  toResponseBody CommentWithUserInfo{..} = CommentInfo {
          id = id
        , parentItem = parentItem
        , commentAuthor = toResponseBody commentAuthor
        , childrenComments = []
        , commentPrio = commentPrio
        , commentText = commentText
        , commentStatus = commentStatus
        }

data CommentBodyInfo = CommentBodyInfo {
    parentItem :: ParentItemIdentifier
  , parentComment :: ParentComment
  , commentPrio :: CommentPriority
  , commentText :: Text
  }

type ERDBody = BasicCrudResponseBodyWithAcceptance ERDIdentifier
type FunDepBody = BasicCrudResponseBodyWithValidation FunDepIdentifier
type RelSchemaBody = BasicCrudResponseBodyWithValidation RelSchemaIdentifier
type PhysSchemaBody = BasicCrudResponseBodyWithAcceptanceAndValidation PhysSchemaIdentifier
