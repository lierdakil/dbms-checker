{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.Main.PredefinedTopics where

import Config
import DB.Types
import DB.Instances ()
import DB.Accessor
import TutorialD.QQ

predefinedTopics :: SessionEnv [PredefinedTopic]
predefinedTopics = fromRelation =<< execDBRel [tutdrel|PredefinedTopic|]
