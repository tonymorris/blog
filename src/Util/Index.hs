{-# LANGUAGE NoImplicitPrelude #-}

module Util.Index(
  niceRoute
, removeIndexHtml
) where

import Control.Monad(return)
import Data.Bool(not)
import Data.Functor(fmap)
import Data.Function(($))
import Data.List(isInfixOf)
import Data.String(String)
import Hakyll(Routes, Item, Compiler, toFilePath, withUrls, customRoute)
import System.FilePath(takeBaseName, splitFileName, takeDirectory, (</>))

-- These tips come from here:
-- http://yannesposito.com/Scratch/en/blog/Hakyll-setup/

-- replace a foo/bar.md by foo/bar/index.html
-- this way the url looks like: foo/bar in most browsers
niceRoute ::
  Routes
niceRoute =
  let
    createIndexRoute ident =
      let
        p = toFilePath ident
      in
        takeDirectory p </> takeBaseName p </> "index.html"
  in
    customRoute createIndexRoute

-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml ::
  Item String
  -> Compiler (Item String)
removeIndexHtml item =
  return $ fmap (withUrls removeIndexStr) item
  where
    removeIndexStr ::
      String
      -> String
    removeIndexStr url =
      case splitFileName url of
        (dir, "index.html") | isLocal dir -> dir
        _                                 -> url
        where
          isLocal uri =
            not (isInfixOf "://" uri)
