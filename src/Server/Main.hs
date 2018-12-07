{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Main where

import Config
import API
import API.Types
import Servant
import Data.Text

import qualified Servant.Auth.Server as Auth
import Server.Main.PredefinedTopics
import Server.Main.CustomTopics
import Server.Main.Users
import Server.Main.ERD
import Server.Main.FunDeps
import Server.Main.RelSchemas
import Server.Main.PhysSchemas
import Server.Main.Comments

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

render :: Text -> Text -> SessionEnv FileData
render "erd" src = undefined
render "fundep" src = undefined
render _ _ = throwError err404
