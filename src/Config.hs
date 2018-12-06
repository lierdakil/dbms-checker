{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Config (
    asks
  , Env(..)
  , Config(..)
  , readConfigFromEnv
  , convertServer
  ) where

import System.Environment
import qualified Data.Map as M
import Data.Maybe
import Data.ByteString (ByteString)
import Control.Monad.Reader
import Control.Monad.Except
import Servant
import Servant.Utils.Enter

newtype Env a = Env {
    runEnv :: ReaderT Config (ExceptT ServantErr IO) a
  } deriving ( Functor, Applicative, Monad, MonadReader Config,
               MonadError ServantErr, MonadIO)

data Config = Config {
    configDataDir    :: !FilePath
  , configStaticDir  :: !FilePath
  , configDataUri    :: !String
  , configPort       :: !Int
  , configOrigins    :: !(Maybe [String])
  , configUserFile   :: !(Maybe FilePath)
  , configNewCmdFile :: !(Maybe FilePath)
  }

convertHandler :: Config -> Env :~> Handler
convertHandler cfg = NT (Handler . flip runReaderT cfg . runEnv)

convertServer :: Enter (Entered Handler Env t) Env Handler t
              => Config -> Entered Handler Env t -> t
convertServer cfg = enter (convertHandler cfg)

readConfigFromEnv :: IO Config
readConfigFromEnv = do
  env <- M.fromList <$> getEnvironment
  return Config {
    configDataDir    = fromMaybe "data" $ M.lookup "MARKCO_DATA_DIR" env
  , configStaticDir  = fromMaybe "../client/dist" $ M.lookup "MARKCO_STATIC_DIR" env
  , configDataUri    = fromMaybe "/data" $ M.lookup "MARKCO_DATA_URI" env
  , configPort       = fromMaybe 8081 (read <$> M.lookup "MARKCO_PORT" env)
  , configOrigins    = words <$> M.lookup "MARKCO_ORIGINS" env
  , configUserFile   = M.lookup "MARKCO_USER_FILE" env
  , configNewCmdFile = M.lookup "MARKCO_NEWCOMMANDS" env
  }
