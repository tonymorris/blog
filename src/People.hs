{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module People(
  peopleRules
) where

import Control.Category((.))
import Control.Monad((=<<), (>>=), return)
import Data.Function(($))
import Data.Functor(fmap)
import Data.Functor.Identity(Identity(Identity))
import Data.List(null)
import Data.Maybe(Maybe(Just, Nothing), maybe)
import Data.Monoid(mappend, mempty)
import Data.String(String)
import Hakyll((.&&.), hasVersion, capture, setExtension, getUnderlying, compile, route, create, match, relativizeUrls, loadAndApplyTemplate, constField, defaultContext, listField, loadAll, defaultContext, Compiler, Item, Rules, makeItem)
import People.Common(lookupAuthors, lookupAuthor)
import Posts.Context(postCtx)
import Util.Index(removeIndexHtml, niceRoute)
import Util.Items(getItems)
import Util.Nick(mkNickCtx)
import Util.Order(orderItems)

getPeoplePosts :: 
  String
  -> Compiler [Item String]
getPeoplePosts =
  getItems lookupAuthors (loadAll "posts/**")

getPeopleTalks ::
  String
  -> Compiler [Item String]
getPeopleTalks =
  getItems (fmap (fmap Identity) . lookupAuthor) (loadAll $ "talks/*" .&&. hasVersion "snippets")

peopleRules ::
  Compiler (Item String)
  -> Rules ()
peopleRules pc = do
  match "snippets/people/*/page.md" $ do
    route $ setExtension "html"
    compile $ do
      nickCtx <- mkNickCtx "snippets/people/*/page.md"
      let peopleCtx = nickCtx `mappend` defaultContext
      pc
        >>= loadAndApplyTemplate "templates/people-page-snippet.html" peopleCtx

  match "snippets/people/*/posts.md" $ do
    route $ setExtension "html"
    compile $ do
      nickCtx <- mkNickCtx "snippets/people/*/posts.md"
      let peopleCtx = nickCtx `mappend` defaultContext
      pc
        >>= loadAndApplyTemplate "templates/people-posts-snippet.html" peopleCtx

  create ["people.html"] $ do
    route niceRoute
    compile $ do
      authors <- orderItems =<< loadAll "snippets/people/*/page.md"
      let authorsCtx =
              constField "people-active" ""                       `mappend`
              listField "authors" defaultContext (return authors) `mappend`
              constField "title" "People"                         `mappend`
              defaultContext

      makeItem ""
          >>= loadAndApplyTemplate "templates/people.html" authorsCtx
          >>= loadAndApplyTemplate "templates/default.html" authorsCtx
          >>= relativizeUrls
      >>= removeIndexHtml

  match "people/*" $ do
    route niceRoute
    compile $ do
      identifier <- getUnderlying
      let
        mIdent =
          case capture "people/*.*" identifier of
            Just [ident, _] -> Just ident
            _ -> Nothing
      peopleTalks <- maybe (return []) getPeopleTalks mIdent
      peoplePosts <- maybe (return []) getPeoplePosts mIdent

      let
        peopleTalkCtx =
          if null peopleTalks then mempty else listField "talks" postCtx (return peopleTalks)
        peoplePostCtx =
          if null peoplePosts then mempty else listField "posts" postCtx (return peoplePosts)
        peopleCtx =
          constField "people-active" "" `mappend`
          peopleTalkCtx                 `mappend`
          peoplePostCtx                 `mappend`
          defaultContext

      pc
        >>= loadAndApplyTemplate "templates/person.html"  peopleCtx
        >>= loadAndApplyTemplate "templates/default.html" peopleCtx
        >>= relativizeUrls
        >>= removeIndexHtml
