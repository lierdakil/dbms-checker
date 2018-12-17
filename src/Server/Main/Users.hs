{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Main.Users where

import Data.Text (Text)
import Control.Monad.Reader
import Config
import API
import API.Types
import DB.Types
import Servant
import DB.Instances ()
import DB.Accessor
import TutorialD.QQ
import Data.UUID hiding (null)
import Data.ByteString.Lazy (fromStrict)

users :: ServerT UsersAPI SessionEnv
users = getUsers
   :<|> \userId ->
        (getUsersTopic userId
   :<|> patchUsersTopic userId)
   :<|> getUsersErd userId
   :<|> getUsersFundep userId
   :<|> getUsersRelSchema userId
   :<|> getUsersSqlSchema userId

getUsers :: Maybe Text -> SessionEnv [UserInfo]
getUsers Nothing = bracketDB $ do
  role <- asks (userSessionUserRole . sessionData)
  when (role/=Teacher) $ throwError err403
  (users' :: [User]) <- fromRelation =<< execDB [tutdrel|User|]
  return $ toResponseBody users'
getUsers (Just grp) = bracketDB $ do
  role <- asks (userSessionUserRole . sessionData)
  when (role/=Teacher) $ throwError err403
  let group = Group grp
  (users' :: [User]) <- fromRelation =<< execDB [tutdrel|User where group = $group |]
  return $ toResponseBody users'

getUsersTopic :: UserIdentifier -> SessionEnv (Maybe AssignedTopicInfo)
getUsersTopic = bracketDB . getUsersTopicInternal

getUsersTopicInternal :: UserIdentifier -> DBContextT SessionEnv (Maybe AssignedTopicInfo)
getUsersTopicInternal userId = do
  (uId, role) <- asks (liftM2 (,) userSessionUserId userSessionUserRole . sessionData)
  when (role /= Teacher && uId /= userId) $ throwError err403
  (tas :: [TopicAssignment]) <- fromRelation =<< execDB [tutdrel|TopicAssignment where userId = $userId|]
  if null tas
  then return Nothing
  else Just <$> getAssignedTopicInfo (topic $ head tas)
  where
    getAssignedTopicInfo (PredefinedAssignedTopic tid) = do
      (topic :: [PredefinedTopic]) <- fromRelation=<<execDB [tutdrel|PredefinedTopic where id = $tid|]
      when (null topic) $ throwError err404{errBody = "No topic with id" <> fromStrict ((\(PredefinedTopicIdentifier u) -> toASCIIBytes u) tid)}
      return $ AssignedTopicInfoPredefined $ head topic
    getAssignedTopicInfo (CustomAssignedTopic tid) = do
      (topic :: [CustomTopic]) <- fromRelation=<<execDB [tutdrel|CustomTopic where id = $tid|]
      when (null topic) $ throwError err404{errBody = "No custom topic with id" <> fromStrict ((\(CustomTopicIdentifier u) -> toASCIIBytes u) tid)}
      return $ AssignedTopicInfoCustom $ head topic


patchUsersTopic :: UserIdentifier -> AssignedTopic -> SessionEnv AssignedTopicInfo
patchUsersTopic userId assignedTopic = bracketDB $ do
  (uId, role) <- asks (liftM2 (,) userSessionUserId userSessionUserRole . sessionData)
  when (role /= Teacher && uId /= userId) $ throwError err403
  let ta = TopicAssignment { userId = userId, topic = assignedTopic }
  execDB [tutdctx|TopicAssignment := (TopicAssignment where not userId = $userId) union $ta|]
  topic <- getUsersTopicInternal userId
  case topic of
    Nothing -> throwError err404{errBody = "Did not find the freshly inserted assignment"}
    Just (AssignedTopicInfoCustom t@CustomTopic{topicAuthor}) -> do
      when (topicAuthor /= userId) $ throwError err400
      return $ AssignedTopicInfoCustom t
    Just t -> return t

getUsersErd :: UserIdentifier -> SessionEnv (Maybe ERDBody)
getUsersErd userId = bracketDB $ do
  (uId, role) <- asks (liftM2 (,) userSessionUserId userSessionUserRole . sessionData)
  when (role /= Teacher && uId /= userId) $ throwError err403
  (rel :: [ERDiagram]) <- fromRelation =<< execDB [tutdrel|ERDiagram where userId = $userId|]
  if null rel
  then return Nothing
  else return $ Just $ toResponseBody $ head rel

getUsersFundep :: UserIdentifier -> SessionEnv (Maybe FunDepBody)
getUsersFundep userId = bracketDB $ do
  (uId, role) <- asks (liftM2 (,) userSessionUserId userSessionUserRole . sessionData)
  when (role /= Teacher && uId /= userId) $ throwError err403
  (rel :: [FunctionalDependencies]) <-
    fromRelation =<< execDB [tutdrel|FunctionalDependencies where userId = $userId|]
  if null rel
  then return Nothing
  else return $ Just $ toResponseBody $ head rel

getUsersRelSchema :: UserIdentifier -> SessionEnv (Maybe RelSchemaBody)
getUsersRelSchema userId = bracketDB $ do
  (uId, role) <- asks (liftM2 (,) userSessionUserId userSessionUserRole . sessionData)
  when (role /= Teacher && uId /= userId) $ throwError err403
  (rel :: [RelationalSchema]) <-
    fromRelation =<< execDB [tutdrel|RelationalSchema where userId = $userId|]
  if null rel
  then return Nothing
  else return $ Just $ toResponseBody $ head rel

getUsersSqlSchema :: UserIdentifier -> SessionEnv (Maybe PhysSchemaBody)
getUsersSqlSchema userId = bracketDB $ do
  (uId, role) <- asks (liftM2 (,) userSessionUserId userSessionUserRole . sessionData)
  when (role /= Teacher && uId /= userId) $ throwError err403
  (rel :: [PhysicalSchema]) <-
    fromRelation =<< execDB [tutdrel|PhysicalSchema where userId = $userId|]
  if null rel
  then return Nothing
  else return $ Just $ toResponseBody $ head rel
