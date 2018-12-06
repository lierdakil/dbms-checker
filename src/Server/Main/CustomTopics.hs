{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Server.Main.CustomTopics where

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

customTopics :: ServerT CustomTopicsAPI Env
customTopics = postCustomTopic :<|>
  \tid -> putCustomTopic tid
     :<|> patchCustomTopic tid

postCustomTopic :: Text -> Env AssignedTopicInfo
postCustomTopic topicName = undefined

putCustomTopic :: CustomTopicIdentifier -> Text -> Env AssignedTopicInfo
putCustomTopic tid topicName = undefined

patchCustomTopic :: CustomTopicIdentifier -> AcceptanceState -> Env ()
patchCustomTopic tid acceptanceState = undefined
