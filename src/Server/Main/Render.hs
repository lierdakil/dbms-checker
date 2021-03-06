{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.Main.Render where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Config
import Servant
import API.Types
import DB.Instances ()
import Algo.ERTools.Parse
import Algo.ERTools.GraphViz
import Algo.FDTools.Parse
import Algo.FDTools.GraphViz
import Algo.ERToFD
import Text.Megaparsec
import Control.Monad.IO.Class

import Data.Text

render :: Text -> Text -> SessionEnv FileData
render "erd" src = do
  let eer = parseER (LT.fromStrict src)
  case eer of
    Left err -> throwError err400{errBody = LTE.encodeUtf8 $ LT.pack $ parseErrorPretty' src err}
    Right er -> do
      image <- liftIO $ drawER er
      return $ FileData image
render "fundep" src = do
  let eer = parseGraph (LT.fromStrict src)
  case eer of
    Left err -> throwError err400{errBody = LTE.encodeUtf8 $ LT.pack $ parseErrorPretty' src err}
    Right er -> do
      image <- liftIO $ drawGraph er
      return $ FileData image
render "fundepFromER" src = do
  let eer = parseER (LT.fromStrict src)
  case eer of
    Left err -> throwError err400{errBody = LTE.encodeUtf8 $ LT.pack $ parseErrorPretty' src err}
    Right er -> do
      image <- liftIO $ drawGraph $ erToFDs er
      return $ FileData image
render _ _ = throwError err404
