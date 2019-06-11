{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.Items(
  getItems
) where

import Data.Maybe (catMaybes)
import Hakyll(Item, Compiler, recentFirst)
import Data.String(String)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Foldable(Foldable, elem)
import Data.Function(($))
import Control.Category((.))
import Control.Monad((=<<), return, (>>=))
import Data.Traversable(traverse)

getItems ::
  Foldable t =>
  (Item String -> Compiler (Maybe (t String)))
  -> Compiler [Item String]
  -> String
  -> Compiler [Item String]
getItems lookupParent loadItems parent = do
  items <- recentFirst =<< loadItems
  let itemMatches i = do
        mParents <- lookupParent i
        return $
          mParents >>= \parents ->
            if parent `elem` parents
            then Just i
            else Nothing
  matchingItems <- traverse itemMatches items
  return . catMaybes $ matchingItems
