{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.Main.PredefinedTopics where

import Config
import DB.Types
import API.Types
import DB.Instances ()
import DB.Accessor
import TutorialD.QQ
import Control.Monad

predefinedTopics :: SessionEnv [PredefinedTopic]
predefinedTopics = do
  (uId, userRole) <- asks (
    liftM2 (,) userInfoUserId userInfoUserRole . userSessionUserInfo . sessionData)
  case userRole of
    Teacher -> bracketDB $ fromRelation =<< execDBRel [tutdrel|PredefinedTopic|]
    Student ->  bracketDB $ fromRelation =<< execDBRel
      [tutdrel|(PredefinedTopic:{topic := PredefinedAssignedTopic @id}) not matching (
        ((TopicAssignment join (
          (User rename {id as userId}) matching (User where id = $uId){group}
          )) where not userId = $uId
          ){topic}
        )|]
