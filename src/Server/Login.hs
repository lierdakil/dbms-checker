{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Server.Login where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import API
import Config
import Servant
import Data.Monoid ((<>))
import Network.Wai
import Servant.Server.Experimental.Auth
import Crypto.Random
import GHC.Conc
import Data.Maybe

loginServer :: ServerT LoginAPI ConfigHandler
loginServer = undefined
--
-- authCheck :: AuthMap -> Config -> AuthHandler Request User
-- authCheck m Config{..}
--   | Just _ <- configUserFile =
--   let check Nothing = throwError err401
--       check (Just hsh) =
--         maybe (throwError err403) return =<< liftIO (atomically $ M.lookup hsh m)
--       extractHeader = fmap B64.decodeLenient . lookup "x-markco-authentication" . requestHeaders
--   in mkAuthHandler (check . extractHeader)
--   | otherwise = mkAuthHandler (const . return $ User "guest")
--
-- authServerContext :: AuthMap -> Config -> Context (AuthHandler Request User ': '[])
-- authServerContext m cfg = authCheck m cfg :. EmptyContext
--
-- login :: AuthMap -> AuthData -> ConfigHandler T.Text
-- login m AuthData{..} =
--   asks configUserFile >>= maybe (throwError err400) doCheckUserAuth
--   where
--     isUserAuthCorrect conf = do
--       let hsh = authLogin <> ":" <> authHashedPassword
--       uf <- liftIO $ LT.lines . LT.decodeUtf8 <$> BL.readFile conf
--       return $ LT.fromStrict hsh `elem` uf
--     doCheckUserAuth = (authAction =<<) . isUserAuthCorrect
--     authAction False = throwError err403
--     authAction True = do
--       Just bytes <- liftIO . iterateUntil isJust $ tryMakeSID
--       return $ LT.toStrict $ LT.decodeUtf8 $ BL.fromStrict $ B64.encode bytes
--     tryMakeSID = do
--       (k :: B.ByteString) <- getRandomBytes 64
--       atomically $ do
--         e <- M.lookup k m
--         case e of
--           Nothing -> M.insert (User authLogin) k m >> return (Just k)
--           Just _ -> return Nothing
