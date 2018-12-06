{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Server.Main.RelSchemas where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Set as S
import Control.Monad.Reader
import Config
import API
import API.Types
import DB.Types
import Servant
import Control.Monad.Error.Class
import Data.Monoid ((<>))
import Crypto.Hash (hash, Digest, SHA1)
import Data.Char (isAlphaNum)
import Control.Exception
import System.IO.Error

relschemas :: ServerT (BasicCrud "relschemaId" RelSchemaIdentifier) SessionEnv
relschemas =
        postRelschemas
   :<|> \relschemaId ->
            getRelschemas relschemaId
       :<|> putRelschemas relschemaId

getRelschemas :: RelSchemaIdentifier -> SessionEnv RelSchemaBody
getRelschemas erdId = undefined

postRelschemas :: Text -> SessionEnv RelSchemaBody
postRelschemas = undefined

putRelschemas :: RelSchemaIdentifier -> Text -> SessionEnv RelSchemaBody
putRelschemas = undefined
