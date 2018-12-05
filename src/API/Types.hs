{-# LANGUAGE DuplicateRecordFields, DeriveGeneric, DeriveAnyClass, TypeFamilies, DataKinds #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module API.Types where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema(..))


import Servant (MimeRender, MimeUnrender, OctetStream)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Swagger as S
import Servant.Auth.Server

import DBTypes

data UserSessionData = UserSessionData {
    userSessionUserId :: UserIdentifier
  , userSessionUserName :: T.Text
  , userSessionUserRole :: Role
} deriving (Generic, FromJSON, ToJSON, FromJWT, ToJWT, ToSchema)

data AuthData = AuthData {
    authLogin :: T.Text
  , authPassword :: T.Text
  } deriving (Generic, FromJSON, ToSchema)


data AssignedTopicInfo =
    AssignedTopicInfoPredefined PredefinedTopic
  | AssignedTopicInfoCustom CustomTopic
  deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype FileData = FileData { unFileData :: BL.ByteString }
    deriving newtype (
      MimeRender OctetStream
    , MimeUnrender OctetStream
    )

instance ToSchema FileData where
  declareNamedSchema _ = return $ S.NamedSchema Nothing S.binarySchema

type family BasicCrudResponseBody (idType :: *) (canAccept :: Bool) where
  BasicCrudResponseBody idType 'False = BasicCrudResponseBodyWithoutValidation idType
  BasicCrudResponseBody idType 'True = BasicCrudResponseBodyWithValidation idType

data BasicCrudResponseBodyWithoutValidation idType = BasicCrudResponseBodyWithoutValidation {
    id :: idType
  , description :: Text
  } deriving (Generic, ToJSON, FromJSON)

instance (ToSchema idType) => ToSchema (BasicCrudResponseBodyWithoutValidation idType)

data BasicCrudResponseBodyWithValidation idType = BasicCrudResponseBodyWithValidation {
    id :: idType
  , description :: Text
  , validationErrors :: [Text]
  } deriving (Generic, ToJSON, FromJSON)

instance (ToSchema idType) => ToSchema (BasicCrudResponseBodyWithValidation idType)

data CommentInfo = CommentInfo {
    id :: CommentIdentifier
  , parentItem :: ParentItemIdentifier
  , parentComment :: ParentComment
  , commentAuthor :: Text -- username
  , commentPrio :: CommentPriority
  , commentText :: Text
  , commentStatus :: CommentStatus
  } deriving (Generic, FromJSON, ToJSON, ToSchema)

data CommentBodyInfo = CommentBodyInfo {
    parentItem :: ParentItemIdentifier
  , parentComment :: ParentComment
  , commentPrio :: CommentPriority
  , commentText :: Text
  } deriving (Generic, FromJSON, ToJSON, ToSchema)

data CommentStatusInfo = CommentStatusInfo {
    commentStatus :: CommentStatus
  } deriving (Generic, FromJSON, ToJSON, ToSchema)
