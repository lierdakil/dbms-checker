{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module DB.Accessor (module DB.Accessor, toAtom) where

import Servant
import Config
import ProjectM36.Client
import ProjectM36.Base
import ProjectM36.Tupleable
import ProjectM36.Atomable
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Control.Monad.IO.Class
import Control.Monad.Except

commitDB :: (MonadIO m, MonadError ServantErr m, HasDBConfig m) => m ()
commitDB = do
  (conn, sid) <- getDBConfig
  handle =<< liftIO (commit sid conn)

class ExecDB a where
  type family ExecDBResultType a
  execDB :: (MonadIO m, MonadError ServantErr m, HasDBConfig m) => a -> m (ExecDBResultType a)

instance ExecDB DatabaseContextExpr where
  type instance ExecDBResultType DatabaseContextExpr = ()
  execDB = execDBContext

instance ExecDB RelationalExpr where
  type instance ExecDBResultType RelationalExpr = Relation
  execDB = execDBRel

execDBContext :: (MonadIO m, MonadError ServantErr m, HasDBConfig m) => DatabaseContextExpr -> m ()
execDBContext expr = do
  (conn, sid) <- getDBConfig
  handle =<< liftIO (executeDatabaseContextExpr sid conn expr)

execDBRel :: (MonadIO m, MonadError ServantErr m, HasDBConfig m) => RelationalExpr -> m Relation
execDBRel expr = do
  (conn, sid) <- getDBConfig
  handle =<< liftIO (executeRelationalExpr sid conn expr)

fromRelation :: (MonadIO m, MonadError ServantErr m, HasDBConfig m, Tupleable a) =>
                Relation -> m [a]
fromRelation (Relation _ tupset) = handle $ sequence $ map fromTuple $ asList tupset

handle :: (MonadError ServantErr m, Show a1) =>
          Either a1 a2 -> m a2
handle (Right val) = return val
handle (Left err) =  throwError $ relErrToServantErr err
relErrToServantErr :: Show a => a -> ServantErr
relErrToServantErr err = err500 { errBody = LT.encodeUtf8 $ LT.pack $ show err }
