{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Server.Main.FunDeps where

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


fundeps :: ServerT (BasicCrud "fundepId" FunDepIdentifier) Env
fundeps = postFundeps
   :<|> \fundepId ->
              renderFundeps fundepId
         :<|> getFundeps fundepId
         :<|> putFundeps fundepId

postFundeps :: Text -> Env FunDepBody
postFundeps = undefined

putFundeps :: FunDepIdentifier -> Text -> Env FunDepBody
putFundeps = undefined

patchFundeps :: FunDepIdentifier -> AcceptanceState -> Env ()
patchFundeps = undefined

renderFundeps :: FunDepIdentifier -> Env FileData
renderFundeps = undefined

getFundeps :: FunDepIdentifier -> Env FunDepBody
getFundeps erdId = undefined
