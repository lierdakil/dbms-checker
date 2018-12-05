{-# LANGUAGE FlexibleContexts #-}

module Server (server) where

import Servant
import API
import Config
import Server.Main
import Server.Login
import Server.Swagger
import Servant.Swagger.UI

server :: Config -> Server API
server cfg = convertServer cfg (mainServer :<|> loginServer)
     :<|> swaggerSchemaUIServer swaggerDoc
