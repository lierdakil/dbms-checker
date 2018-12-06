{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Server.Main where

import Config
import API
import API.Types
import Servant

import qualified Servant.Auth.Server as Auth
import Server.Main.PredefinedTopics
import Server.Main.CustomTopics
import Server.Main.Users
import Server.Main.ERD
import Server.Main.FunDeps
import Server.Main.RelSchemas
import Server.Main.PhysSchemas
import Server.Main.Comments

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
