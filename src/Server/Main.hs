{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Server.Main where

import Config
import API
import Servant

import Server.Main.PredefinedTopics
import Server.Main.CustomTopics
import Server.Main.Users
import Server.Main.ERD
import Server.Main.FunDeps
import Server.Main.RelSchemas
import Server.Main.PhysSchemas
import Server.Main.Comments
import Server.Main.Render

mainServer :: ServerT BasicAPI SessionEnv
mainServer =
         predefinedTopics
    :<|> customTopics
    :<|> users
    :<|> erd
    :<|> fundeps
    :<|> relschemas
    :<|> sqlschemas
    :<|> comments
    :<|> render
