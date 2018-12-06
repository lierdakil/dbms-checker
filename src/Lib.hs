{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Prelude

import Network.Wai
import Servant
import Network.Wai.Middleware.Cors
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import API
import Server
import Config
import Servant.Auth.Server
import Crypto.JOSE.JWK

app :: JWK -> Config -> Application
app jwtKey cfg = logStdoutDev
    $ static
    $ cors (const $ Just corspolicy)
    $ serveWithContext api cfgctx (server jwtCfg cfg)
  where
  jwtCfg = defaultJWTSettings jwtKey
  cfgctx = defaultCookieSettings :. jwtCfg :. cfg :. EmptyContext
  corspolicy = simpleCorsResourcePolicy
           { corsRequestHeaders = [ "content-type" ]
           , corsMethods = map renderStdMethod [GET, PUT, POST, PATCH, DELETE, OPTIONS]
           , corsOrigins = Nothing -- TODO: Fix this
           }
