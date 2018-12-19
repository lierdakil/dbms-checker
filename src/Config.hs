{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}

module Config (
    asks
  , Env(..)
  , SessionEnv(..)
  , Config(..)
  , Session(..)
  , HasConfig(..)
  , ntEnv
  , ntSessionEnv
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Catch
import Control.Monad.Base
import Control.Monad.Trans.Control
import Servant
import Data.Time
import ProjectM36.Client (Connection)

import API.Types

newtype Env a = Env {
    runEnv :: ReaderT Config (ExceptT ServantErr IO) a
  } deriving ( Functor, Applicative, Monad, MonadReader Config,
               MonadError ServantErr, MonadIO, MonadThrow, MonadCatch,
               MonadMask, MonadBase IO, MonadBaseControl IO)

newtype SessionEnv a = SessionEnv {
    runSessionEnv :: ReaderT Session (ExceptT ServantErr IO) a
  } deriving ( Functor, Applicative, Monad, MonadReader Session,
               MonadError ServantErr, MonadIO, MonadThrow, MonadCatch,
               MonadMask, MonadBase IO, MonadBaseControl IO)

data Session = Session {
    sessionConfig :: !Config
  , sessionData :: !UserSessionData
  }

class HasConfig (m :: * -> *) where
  getConfigParam :: (Config -> a) -> m a

instance HasConfig Env where
  getConfigParam = asks

instance HasConfig SessionEnv where
  getConfigParam f = asks (f . sessionConfig)

data Config = Config {
    configPort       :: !Int
  , configDBConn     :: !Connection
  , configOrigins    :: !(Maybe [String])
  , configSessionDur :: !NominalDiffTime
  }

ntEnv :: Config -> Env a -> Servant.Handler a
ntEnv cfg (Env r) = Servant.Handler $ runReaderT r cfg

ntSessionEnv :: Config -> UserSessionData -> SessionEnv a -> Servant.Handler a
ntSessionEnv cfg sd (SessionEnv r) = Servant.Handler $ runReaderT r Session {
    sessionConfig = cfg
  , sessionData = sd
  }
