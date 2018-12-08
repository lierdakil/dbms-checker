{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Main.Comments where

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
import ProjectM36.Base
import ProjectM36.Relation
import Data.Time

comments :: ServerT CommentsAPI SessionEnv
comments = postComment
      :<|> getComments
      :<|> \commentId ->
            putComment commentId
       :<|> patchComment commentId

postComment :: CommentBodyInfo -> SessionEnv CommentInfo
postComment CommentBodyInfo{..} = bracketDB $ do
  userInfo@UserInfo{..} <- asks (userSessionUserInfo . sessionData)
  when (userInfoUserRole /= Teacher) $ checkItemOwnership parentItem
  nid <- getNewId CommentIdentifier
  now <- liftIO getCurrentTime
  let comment = Comment {
      id = nid
    , commentTime = now
    , parentItem = parentItem
    , parentComment = parentComment
    , commentAuthor = userInfoUserId
    , commentPrio = commentPrio
    , commentText = commentText
    , commentStatus = CommentStateOpen
    }
  execDB [tutdctx|insert Comment $comment|]
  return $ CommentInfo {
      id = nid
    , commentTime = now
    , parentItem = parentItem
    , commentAuthor = userInfo
    , commentPrio = commentPrio
    , commentText = commentText
    , commentStatus = CommentStateOpen
    , childrenComments = []
    }

getComments :: Maybe ParentItemIdentifier -> SessionEnv [CommentInfo]
getComments Nothing = bracketDB $ do
  userRole <- asks (userSessionUserRole . sessionData)
  when (userRole /= Teacher) $ throwError err403
  (rels :: [CommentWithUserInfo]) <-
    fromRelation =<< execDB [tutdrel|Comment join $userRelationForJoin|]
  return $ toResponseBody rels
getComments (Just parentItem) = bracketDB $ do
  role <- asks (userSessionUserRole . sessionData)
  when (role /= Teacher) $ checkItemOwnership parentItem
  (rels :: [CommentWithUserInfo]) <-
    fromRelation =<< execDB [tutdrel|(Comment where parentItem = $parentItem) join
      $userRelationForJoin|]
  return $ toResponseBody rels

userRelationForJoin :: RelationalExpr
userRelationForJoin = [tutdrel|(User rename {
  id as commentAuthor,
  username as authorUsername,
  group as authorGroup,
  email as authorEmail,
  registrationDate as authorRegistrationDate,
  role as authorRole
  })|]

putComment :: CommentIdentifier -> CommentBodyInfo -> SessionEnv CommentInfo
putComment cid CommentBodyInfo{..} = bracketDB $ do
  userInfo@UserInfo{..} <- asks (userSessionUserInfo . sessionData)
  when (userInfoUserRole /= Teacher) $ checkItemOwnership parentItem
  (comments :: [Comment]) <- fromRelation =<< execDB
    [tutdrel|Comment where commentAuthor = $userInfoUserId and id = $cid|]
  when (null comments) $ throwError err404
  let Comment{commentTime} = head comments
  execDB [tutdctx|update Comment where commentAuthor = $userInfoUserId and id = $cid (
        parentItem := $parentItem
      , commentTime := $commentTime
      , parentComment := $parentComment
      , commentPrio := $commentPrio
      , commentText := $commentText
      ) |]
  return $ CommentInfo {
      id = cid
    , commentTime = commentTime
    , parentItem = parentItem
    , commentAuthor = userInfo
    , commentPrio = commentPrio
    , commentText = commentText
    , commentStatus = CommentStateOpen
    , childrenComments = []
    }

patchComment :: CommentIdentifier -> CommentStatus -> SessionEnv ()
patchComment cid st = bracketDB $ do
  uId <- asks (userSessionUserId . sessionData)
  execDB [tutdctx|update Comment where commentAuthor = $uId and id = $cid (commentStatus := $st)|]

checkItemOwnership :: ParentItemIdentifier -> DBContextT SessionEnv ()
checkItemOwnership parent = do
  -- check if user owns the parent item in question
  uId <- asks (userSessionUserId . sessionData)
  count <- cardinality <$> execDB [tutdrel|(
          relation{tuple{id ParentTopicSelection $uId}}
    union ((ERDiagram where userId = $uId){id}:{id:=ParentERD id})
    union ((FunctionalDependencies where userId = $uId){id}:{id:=ParentFunDep id})
    union ((RelationalSchema where userId = $uId){id}:{id:=ParentRelSchema id})
    union ((PhysicalSchema where userId = $uId){id}:{id:=ParentPhysSchema id})
    ) where id = $parent|]
  when (count /= Finite 1) $ throwError err403
