{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Access files on the filesystem.
module Server.Static
    ( staticServer
    ) where

import qualified Data.Text as T
import Servant
import WaiAppStatic.Storage.Filesystem
import Network.Wai.Application.Static
import WaiAppStatic.Types


staticServer :: FilePath -> Server Raw
staticServer dataDir = serveDirectoryWith settings
  where
  settings = let orig = defaultWebAppSettings dataDir in orig {
      ssLookupFile = lookupFile (ssLookupFile orig)
    }
  lookupFile orig xs
    | not $ null xs
    , any (`T.isSuffixOf` fromPiece (last xs)) [".html", ".css", ".js", ".map", ".svg"]
    = orig [last xs]
    | otherwise = orig [unsafeToPiece "index.html"]
