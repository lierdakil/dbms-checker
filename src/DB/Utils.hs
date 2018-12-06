{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module DB.Utils where

import Servant
import Control.Monad.IO.Class
import Data.UUID
import Data.UUID.V1
import Control.Monad.Except

getNewId :: (MonadIO m, MonadError ServantErr m) => (UUID -> a) -> m a
getNewId constr = do
  uuid <- liftIO $ nextUUID
  case uuid of
    Nothing -> throwError $ ServantErr {
        errHTTPCode = 429
      , errReasonPhrase = "Too Many Requests"
      , errBody = "UUID was not generated"
      , errHeaders = []
      }
    Just i -> return $ constr i
