{-# LANGUAGE FlexibleContexts, DataKinds, TypeOperators #-}

module Server (server) where

import Servant
import API
import API.Types
import Config
import Server.Main
import Server.Login
import Server.Swagger
import Server.Static
import Servant.Swagger.UI
import Servant.Auth.Server as Auth
import ProjectM36.Client

authServer :: Config -> AuthResult UserSessionData -> Server BasicAPI
authServer cfg (Auth.Authenticated sess) =
  hoistServerWithContext basicApi context (ntSessionEnv cfg sess) mainServer
authServer _ _ = throwAll err401

server :: JWTSettings -> Config -> Server API
server jwt cfg = authServer cfg
    :<|> hoistServerWithContext loginApi context (ntEnv cfg) (loginServer jwt)
    :<|> swaggerSchemaUIServer swaggerDoc
    :<|> staticServer

context :: Proxy '[CookieSettings, JWTSettings]
context = Proxy
