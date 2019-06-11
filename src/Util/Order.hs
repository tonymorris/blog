{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.Order(
  orderItems
) where

import Control.Category((.))
import Control.Monad(return)
import qualified Data.Aeson as Aeson(Value(Number, String))
import Data.Functor(fmap)
import Data.Function(($))
import qualified Data.HashMap.Lazy as HashMap(lookup)
import Data.Int(Int)
import Data.List(sortBy, zip)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ordering(GT, EQ, LT), compare)
import Data.Traversable(traverse)
import Data.Tuple(fst, snd)
import qualified Data.Scientific as Scientific(toBoundedInteger)
import qualified Data.Text as Text(unpack)
import Text.Read (readMaybe)
import Hakyll(Item, MonadMetadata, itemIdentifier, getMetadata)

comp ::
  Maybe Int
  -> Maybe Int
  -> Ordering
comp (Just x) (Just y) =
  compare x y
comp Nothing (Just _) =
  GT
comp (Just _) Nothing =
  LT
comp Nothing Nothing =
  EQ

lookupOrder ::
  MonadMetadata m =>
  Item a
  -> m (Maybe Int)
lookupOrder item = do
  metadata <- getMetadata (itemIdentifier item)
  return $ case HashMap.lookup "order" metadata of
    Just (Aeson.Number n) ->
      Scientific.toBoundedInteger n
    Just (Aeson.String p) ->
      readMaybe . Text.unpack $ p
    _ ->
      Nothing

orderItems ::
  MonadMetadata m =>
  [Item a]
  -> m [Item a]
orderItems items = do
  orders <- traverse lookupOrder items
  return .
    fmap fst .
    sortBy (\x y -> comp (snd x) (snd y)) .
    zip items $
    orders
