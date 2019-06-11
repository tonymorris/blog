{-# LANGUAGE NoImplicitPrelude #-}

module Posts.Context(
  postCtx
) where

import Control.Category((.))
import Control.Monad(return)
import Data.Function(($))
import Data.Functor(fmap)
import Data.Maybe(Maybe, maybe)
import Data.Monoid(mappend)
import Data.String(String)
import Hakyll(MonadMetadata, Context, Item(Item), dateField, fromFilePath, defaultContext, listFieldWith, splitAll, trim, itemIdentifier, getMetadataField)

lookupExtras ::
  MonadMetadata m =>
  String
  -> Item a
  -> m (Maybe [String])
lookupExtras tag item = do
  mExtras <- getMetadataField (itemIdentifier item) tag
  return $ fmap (fmap trim . splitAll ",") mExtras

extras ::
  String
  -> Context a
extras tag = listFieldWith tag defaultContext $ \item -> do
  es <- lookupExtras tag item
  return $ maybe [] (fmap (\l -> Item (fromFilePath l) l)) es

postCtx ::
  Context String
postCtx =
  extras "extra-css" `mappend`
  extras "extra-js" `mappend`
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext
