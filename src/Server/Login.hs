{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Server.Login where

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import API
import Config
import Servant
import Servant.Auth.Server
import Crypto.KDF.BCrypt
import API.Types
import DB.Types
import DB.Instances ()
import DB.Accessor
import Control.Monad.IO.Class
import TutorialD.QQ
import Control.Monad
import Data.Time

loginServer :: JWTSettings -> ServerT LoginAPI Env
loginServer = checkCreds

checkCreds :: JWTSettings
           -> AuthData
           -> Env UserSessionData
checkCreds jwtCfg AuthData{..} = bracketDB $ do
  (rel :: [User]) <- fromRelation =<< execDB [tutdrel|User where username = $authLogin|]
  when (null rel) $ throwError err
  let User{saltedPasswordHash} = head rel
  let bspw = BL8.toStrict $ LTE.encodeUtf8 $ LT.fromStrict authPassword
      bshs = saltedPasswordHash
  when (not $ validatePassword bspw bshs) $ throwError err
  let usr = UserSessionData {
        userSessionUserInfo = toResponseBody $ head rel
      , userSessionKey = ""
      }
  sd <- asks configSessionDur
  expirationDateTime <- Just . addUTCTime sd <$> liftIO getCurrentTime
  key <- BL8.unpack <$> (handle =<< liftIO (makeJWT usr jwtCfg expirationDateTime))
  return usr {userSessionKey = key}
  where
    err = err401{
      errBody = LTE.encodeUtf8 $ "Неверное имя пользователя и/или пароль"
      }
