{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Main.CustomTopics where

import Data.Text (Text)
import Control.Monad.Reader
import Config
import API
import API.Types
import DB.Types
import Servant
import DB.Instances ()
import DB.Accessor
import DB.Utils
import TutorialD.QQ

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
  let assignment = TopicAssignment { userId = uId, topic = CustomAssignedTopic nid }
  execDB [tutdctx|TopicAssignment := TopicAssignment where not userId = $uId
    union $assignment|]
  commitDB
  return $ AssignedTopicInfoCustom topic

putCustomTopic :: CustomTopicIdentifier -> Text -> SessionEnv (Maybe AssignedTopicInfo)
putCustomTopic tid topicName = do
  uId <- asks (userSessionUserId . sessionData)
  execDB [tutdctx|update CustomTopic where id = $tid and userId = $uId (
    name := $topicName, accepted := NotAccepted )|]
  commitDB
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
patchCustomTopic tid acceptanceState = do
  userRole <- asks (userSessionUserRole . sessionData)
  when (userRole /= Teacher) $ throwError err403
  execDB [tutdctx|update CustomTopic where id = $tid (accepted := $acceptanceState)|]
  commitDB
