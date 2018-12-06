{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Server.Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Set as S
import Control.Monad.Reader
import Config
import API
import API.Types
import DB.Types
import Servant
import Control.Monad.Error.Class
import Data.Monoid ((<>))
import Crypto.Hash (hash, Digest, SHA1)
import Data.Char (isAlphaNum)
import Control.Exception
import System.IO.Error

import qualified Servant.Auth.Server as Auth

type ERDBody = BasicCrudResponseBodyWithAcceptance ERDIdentifier
type FunDepBody = BasicCrudResponseBodyWithValidation FunDepIdentifier
type RelSchemaBody = BasicCrudResponseBodyWithValidation RelSchemaIdentifier
type PhysSchemaBody = BasicCrudResponseBodyWithAcceptanceAndValidation PhysSchemaIdentifier

mainServer :: ServerT MainAPI Env
mainServer (Auth.Authenticated UserSessionData{..}) =
         predefinedTopics
    :<|> customTopics
    :<|> users
    :<|> erd
    :<|> fundeps
    :<|> relschemas
    :<|> sqlschemas
    :<|> comments
mainServer _ = Auth.throwAll err401

customTopics :: ServerT CustomTopicsAPI Env
customTopics = postCustomTopic :<|>
  \tid -> putCustomTopic tid
     :<|> patchCustomTopic tid

users :: ServerT UsersAPI Env
users userId =
        (getUsersTopic userId
   :<|> patchUsersTopic userId)
   :<|> getUsersErd userId
   :<|> getUsersFundep userId
   :<|> getUsersRelSchema userId
   :<|> getUsersSqlSchema userId

erd :: ServerT (BasicCrud "erdId" ERDIdentifier) Env
erd = postErd
  :<|> erdManip
  where
    erdManip erdId =
              renderErd erdId
         :<|> patchErd erdId
         :<|> getErd erdId
         :<|> putErd erdId
fundeps :: ServerT (BasicCrud "fundepId" FunDepIdentifier) Env
fundeps = postFundeps
   :<|> \fundepId ->
              renderFundeps fundepId
         :<|> getFundeps fundepId
         :<|> putFundeps fundepId
relschemas :: ServerT (BasicCrud "relschemaId" RelSchemaIdentifier) Env
relschemas =
        postRelschemas
   :<|> \relschemaId ->
            getRelschemas relschemaId
       :<|> putRelschemas relschemaId
sqlschemas :: ServerT (BasicCrud "sqlschemaId" PhysSchemaIdentifier) Env
sqlschemas = postSqlschemas
   :<|> \sqlSchemaId ->
              patchSqlschemas sqlSchemaId
         :<|> getSqlschemas sqlSchemaId
         :<|> putSqlschemas sqlSchemaId
comments :: ServerT CommentsAPI Env
comments = postComment
      :<|> getComments
      :<|> \commentId ->
            putComment commentId
       :<|> patchComment commentId

predefinedTopics :: Env [PredefinedTopic]
predefinedTopics = undefined

postCustomTopic :: Text -> Env AssignedTopicInfo
postCustomTopic topicName = undefined

putCustomTopic :: CustomTopicIdentifier -> Text -> Env AssignedTopicInfo
putCustomTopic tid topicName = undefined

patchCustomTopic :: CustomTopicIdentifier -> AcceptanceState -> Env ()
patchCustomTopic tid acceptanceState = undefined

getUsersTopic :: UserIdentifier -> Env AssignedTopicInfo
getUsersTopic userId = undefined

patchUsersTopic :: UserIdentifier -> AssignedTopic -> Env AssignedTopicInfo
patchUsersTopic userId assignedTopic = undefined

getUsersErd :: UserIdentifier -> Env ERDBody
getUsersErd userId = undefined

getUsersFundep :: UserIdentifier -> Env FunDepBody
getUsersFundep userId = undefined

getUsersRelSchema :: UserIdentifier -> Env RelSchemaBody
getUsersRelSchema userId = undefined

getUsersSqlSchema :: UserIdentifier -> Env PhysSchemaBody
getUsersSqlSchema userId = undefined

getErd :: ERDIdentifier -> Env ERDBody
getErd erdId = undefined

postErd :: Text -> Env ERDBody
postErd = undefined

putErd :: ERDIdentifier -> Text -> Env ERDBody
putErd = undefined

patchErd :: ERDIdentifier -> AcceptanceState -> Env ()
patchErd = undefined

renderErd :: ERDIdentifier -> Env FileData
renderErd = undefined

getFundeps :: FunDepIdentifier -> Env FunDepBody
getFundeps erdId = undefined

postFundeps :: Text -> Env FunDepBody
postFundeps = undefined

putFundeps :: FunDepIdentifier -> Text -> Env FunDepBody
putFundeps = undefined

patchFundeps :: FunDepIdentifier -> AcceptanceState -> Env ()
patchFundeps = undefined

renderFundeps :: FunDepIdentifier -> Env FileData
renderFundeps = undefined

getRelschemas :: RelSchemaIdentifier -> Env RelSchemaBody
getRelschemas erdId = undefined

postRelschemas :: Text -> Env RelSchemaBody
postRelschemas = undefined

putRelschemas :: RelSchemaIdentifier -> Text -> Env RelSchemaBody
putRelschemas = undefined

getSqlschemas :: PhysSchemaIdentifier -> Env PhysSchemaBody
getSqlschemas erdId = undefined

postSqlschemas :: Text -> Env PhysSchemaBody
postSqlschemas = undefined

putSqlschemas :: PhysSchemaIdentifier -> Text -> Env PhysSchemaBody
putSqlschemas = undefined

patchSqlschemas :: PhysSchemaIdentifier -> AcceptanceState -> Env ()
patchSqlschemas = undefined

postComment :: CommentBodyInfo -> Env CommentInfo
postComment = undefined

getComments :: Maybe ParentItemIdentifier -> Env [CommentInfo]
getComments = undefined

putComment :: CommentIdentifier -> CommentBodyInfo -> Env CommentInfo
putComment = undefined

patchComment :: CommentIdentifier -> CommentStatusInfo -> Env ()
patchComment = undefined
