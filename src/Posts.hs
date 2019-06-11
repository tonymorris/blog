{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Posts(
  postRules
) where

import Control.Monad((>>=), (=<<), return)
import Data.Function(($))
import Data.Monoid(mappend)
import Data.String(String)
import Hakyll(recentFirst, compile, idRoute, route, create, match, relativizeUrls, loadAndApplyTemplate, constField, defaultContext, listField, loadAll, defaultContext, Compiler, Item, Rules, saveSnapshot, makeItem)
import People.Context(authorFieldCtx)
import Posts.Context(postCtx)
import Util.Index(niceRoute, removeIndexHtml)

postRules ::
  Compiler (Item String)
  -> Rules ()
postRules pc = do
  match "posts/**" $ do
    route niceRoute
    compile $ do
      let
          projectCtx =
            authorFieldCtx `mappend` postCtx
      pc
        >>= saveSnapshot "post-content"
        >>= loadAndApplyTemplate "templates/post.html"    projectCtx
        >>= loadAndApplyTemplate "templates/default.html" projectCtx
        >>= relativizeUrls
        >>= removeIndexHtml

  match "drafts/**" $ do
    route niceRoute
    compile $ do
      let
          projectCtx =
            authorFieldCtx `mappend` postCtx
      pc
        >>= loadAndApplyTemplate "templates/post.html"    projectCtx
        >>= loadAndApplyTemplate "templates/default.html" projectCtx
        >>= relativizeUrls
        >>= removeIndexHtml

  match "links/**" $ do
    route niceRoute
    compile $ do
      let
          projectCtx =
            authorFieldCtx `mappend` postCtx
      pc
        >>= loadAndApplyTemplate "templates/post.html"    projectCtx
        >>= loadAndApplyTemplate "templates/default.html" projectCtx
        >>= relativizeUrls
        >>= removeIndexHtml

  create ["archive/index.html"] $ do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll "posts/**"
        let archiveCtx =
              constField "archive-active" ""           `mappend`
              listField "posts" postCtx (return posts) `mappend`
              constField "title" "Archives"            `mappend`
              defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls
          >>= removeIndexHtml
