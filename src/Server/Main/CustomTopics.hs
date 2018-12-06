{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Main.CustomTopics where

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
import Config
import DB.Types
import DB.Instances ()
import DB.Accessor
import DB.Utils
import TutorialD.QQ
import Data.UUID.V1
import Data.Maybe

customTopics :: ServerT CustomTopicsAPI SessionEnv
customTopics = postCustomTopic :<|>
  \tid -> putCustomTopic tid
     :<|> patchCustomTopic tid

postCustomTopic :: Text -> SessionEnv AssignedTopicInfo
postCustomTopic topicName = do
  nid <- getNewId CustomTopicIdentifier
  uId <- asks (userSessionUserId . sessionData)
  let topic = CustomTopic {
    id = nid
  , name = topicName
  , topicAuthor = uId
  , accepted = NotAccepted
  }
  execDB [tutdctx|insert CustomTopic $topic|]
  execDB [tutdctx|update TopicAssignment where userId = $uId (topic:=CustomAssignedTopic $nid)|]
  dbCommit
  return $ AssignedTopicInfoCustom topic

putCustomTopic :: CustomTopicIdentifier -> Text -> SessionEnv (Maybe AssignedTopicInfo)
putCustomTopic tid topicName = do
  execDB [tutdctx|update CustomTopic where id = $tid (
    name := $topicName, accepted := Accepted )|]
  dbCommit
  uId <- asks (userSessionUserId . sessionData)
  (tasgn :: [TopicAssignment]) <- fromRelation =<< execDB [tutdrel|TopicAssignment where userId = $uId|]
  if null tasgn
  then return Nothing
  else do
    let tas = head tasgn
    case topic tas of
      (PredefinedAssignedTopic ttid) -> tryHead AssignedTopicInfoPredefined <$>
        (fromRelation =<< execDB [tutdrel|PredefinedTopic where id = $ttid|])
      (CustomAssignedTopic ttid) -> tryHead AssignedTopicInfoCustom <$>
        (fromRelation =<< execDB [tutdrel|CustomTopic where id = $ttid|])
  where
    tryHead _ [] = Nothing
    tryHead c (x:_) = Just (c x)

patchCustomTopic :: CustomTopicIdentifier -> AcceptanceState -> SessionEnv ()
patchCustomTopic tid acceptanceState = undefined
