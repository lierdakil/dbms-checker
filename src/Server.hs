{-# LANGUAGE FlexibleContexts, DataKinds, TypeOperators #-}

module Server (server) where

import Servant
import API
import API.Types
import Config
import Server.Main
import Server.Login
import Server.Swagger
import Servant.Swagger.UI
import Servant.Auth.Server as Auth

authServer :: Config -> AuthResult UserSessionData -> Server BasicAPI
authServer cfg (Auth.Authenticated sess) =
  hoistServerWithContext basicApi context (ntSessionEnv cfg sess) mainServer
authServer _ _ = throwAll err401

server :: Config -> Server API
server cfg = authServer cfg
  :<|> hoistServerWithContext loginApi context (ntEnv cfg) loginServer
  :<|> swaggerSchemaUIServer swaggerDoc

context :: Proxy '[CookieSettings, JWTSettings]
context = Proxy
