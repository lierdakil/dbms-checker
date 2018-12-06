{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Server.Main.Users where

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

users :: ServerT UsersAPI Env
users userId =
        (getUsersTopic userId
   :<|> patchUsersTopic userId)
   :<|> getUsersErd userId
   :<|> getUsersFundep userId
   :<|> getUsersRelSchema userId
   :<|> getUsersSqlSchema userId

getUsersTopic :: UserIdentifier -> Env AssignedTopicInfo
getUsersTopic userId = undefined

patchUsersTopic :: UserIdentifier -> AssignedTopic -> Env AssignedTopicInfo
patchUsersTopic userId assignedTopic = undefined

getUsersErd :: UserIdentifier -> Env ERDBody
getUsersErd userId = undefined

getUsersFundep :: UserIdentifier -> Env FunDepBody
getUsersFundep userId = undefined

getUsersRelSchema :: UserIdentifier -> Env RelSchemaBody
getUsersRelSchema userId = undefined

getUsersSqlSchema :: UserIdentifier -> Env PhysSchemaBody
getUsersSqlSchema userId = undefined
