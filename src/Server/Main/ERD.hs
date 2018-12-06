{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Server.Main.ERD where

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


erd :: ServerT (BasicCrud "erdId" ERDIdentifier) SessionEnv
erd = postErd
  :<|> erdManip
  where
    erdManip erdId =
              renderErd erdId
         :<|> patchErd erdId
         :<|> getErd erdId
         :<|> putErd erdId

getErd :: ERDIdentifier -> SessionEnv ERDBody
getErd erdId = undefined

postErd :: Text -> SessionEnv ERDBody
postErd = undefined

putErd :: ERDIdentifier -> Text -> SessionEnv ERDBody
putErd = undefined

patchErd :: ERDIdentifier -> AcceptanceState -> SessionEnv ()
patchErd = undefined

renderErd :: ERDIdentifier -> SessionEnv FileData
renderErd = undefined
