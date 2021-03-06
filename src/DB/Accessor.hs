{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DB.Accessor (module DB.Accessor, toAtom) where

import Servant
import Config
import ProjectM36.Client
import ProjectM36.Base
import ProjectM36.Tupleable
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control

import Control.Monad.State
import Control.Monad.Catch hiding (handle, onException)
import Control.Monad.Base

newtype DBContextT m a = DBContextT {
  runDBContextT :: ReaderT (Connection, SessionId) (StateT Bool m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow,
              MonadCatch)
deriving instance MonadError ServantErr m => MonadError ServantErr (DBContextT m)
deriving instance MonadBase IO m => MonadBase IO (DBContextT m)
deriving instance MonadBaseControl IO m => MonadBaseControl IO (DBContextT m)
instance MonadTrans DBContextT where
  lift a = DBContextT $ ReaderT $ const $ lift a

instance MonadReader r m => MonadReader r (DBContextT m) where
  ask = lift ask
  local f a = DBContextT . ReaderT $ \c -> local f $ runReaderT (runDBContextT a) c

getDBConfig :: Monad m => DBContextT m (Connection, SessionId)
getDBConfig = DBContextT ask

setDirty :: Monad m => Bool -> DBContextT m ()
setDirty b = DBContextT $ put b

getIsDirty :: Monad m => DBContextT m Bool
getIsDirty = DBContextT get

commitDB :: (MonadBaseControl IO m, MonadIO m, MonadError ServantErr m) => DBContextT m ()
commitDB = do
  (conn, sid) <- getDBConfig
  handle =<< liftIO (commit sid conn)
  setDirty False

rollbackDB :: (MonadBaseControl IO m, MonadIO m, MonadError ServantErr m) => DBContextT m ()
rollbackDB = do
  (conn, sid) <- getDBConfig
  handle =<< liftIO (rollback sid conn)
  setDirty False

bracketDB :: (HasConfig m, MonadMask m, MonadBaseControl IO m, MonadIO m, MonadError ServantErr m) => DBContextT m a -> m a
bracketDB ma = bracket getConn freeConn $ flip evalStateT False . runReaderT (runDBContextT action)
  where
    action = do
      x <- ma
      isDirty <- getIsDirty
      when isDirty commitDB
      return x
    getConn = do
      conn <- getConfigParam configDBConn
      sess <- handle =<< liftIO (createSessionAtHead conn "master")
      return (conn, sess)
    freeConn (conn, sess) = liftIO $
      closeSession sess conn

class ExecDB a where
  type family ExecDBResultType a
  execDB :: (MonadIO m, MonadError ServantErr m) => a -> DBContextT m (ExecDBResultType a)

instance ExecDB DatabaseContextExpr where
  type ExecDBResultType DatabaseContextExpr = ()
  execDB = execDBContext

instance ExecDB RelationalExpr where
  type ExecDBResultType RelationalExpr = Relation
  execDB = execDBRel

execDBContext :: (MonadIO m, MonadError ServantErr m) => DatabaseContextExpr -> DBContextT m ()
execDBContext expr = do
  (conn, sid) <- getDBConfig
  handle =<< liftIO (executeDatabaseContextExpr sid conn expr)
  setDirty True

execDBRel :: (MonadIO m, MonadError ServantErr m) => RelationalExpr -> DBContextT m Relation
execDBRel expr = do
  (conn, sid) <- getDBConfig
  handle =<< liftIO (executeRelationalExpr sid conn expr)

fromRelation :: (MonadIO m, MonadError ServantErr m, Tupleable a) =>
                Relation -> DBContextT m [a]
fromRelation (Relation _ tupset) = handle $ mapM fromTuple $ asList tupset

handle :: (MonadError ServantErr m, Show a1) =>
          Either a1 a2 -> m a2
handle (Right val) = return val
handle (Left err) =  throwError $ relErrToServantErr err
relErrToServantErr :: Show a => a -> ServantErr
relErrToServantErr err = err500 { errBody = LT.encodeUtf8 $ LT.pack $ show err }
