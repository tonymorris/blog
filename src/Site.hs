{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Category((.))
import Control.Monad(return, (>>=), (=<<))
import Data.Bool(Bool(False))
import Data.Function(($))
import Data.Functor(fmap)
import Data.List(take, drop)
import Data.Monoid(mappend)
import Hakyll(feedRoot, feedAuthorName, feedDescription, feedAuthorEmail, feedTitle, FeedConfiguration(FeedConfiguration), Configuration(destinationDirectory, ignoreFile), renderRss, loadAllSnapshots, renderAtom, recentFirst, compile, bodyField, idRoute, route, create, templateBodyCompiler, match, relativizeUrls, loadAndApplyTemplate, constField, applyAsTemplate, getResourceBody, defaultContext, listField, loadAll, setExtension, defaultContext, copyFileCompiler, compressCssCompiler, hakyllWith, defaultConfiguration, customRoute, toFilePath)
import People(peopleRules)
import Posts(postRules)
import Posts.Context(postCtx)
import System.IO(IO)
import Util.Index(niceRoute, removeIndexHtml)
import Util.Pandoc(pandocCompiler')
import System.FilePath

config ::
  Configuration
config =
  defaultConfiguration {
    destinationDirectory = "public"
  , ignoreFile = \_ -> False
  }

main ::
  IO ()
main = do
  hakyllWith config $ do
    match "images/**" $ do
      route   idRoute
      compile copyFileCompiler

    match "fonts/**" $ do
      route   idRoute
      compile copyFileCompiler

    match "js/**" $ do
      route   idRoute
      compile copyFileCompiler

    match "css/**" $ do
      route   idRoute
      compile compressCssCompiler

    match "share/**" $ do
      route   (customRoute (joinPath . drop 1 . splitDirectories . toFilePath))
      compile copyFileCompiler

    match "location.html" $ do
      route niceRoute
      compile $ do
        let locationCtx =
              constField "location-active" "" `mappend` defaultContext
        getResourceBody
          >>= loadAndApplyTemplate "templates/default.html" locationCtx
          >>= relativizeUrls
          >>= removeIndexHtml

    match "contact.html" $ do
      route niceRoute
      compile $ do
        let contactCtx =
              constField "contact-active" "true" `mappend` defaultContext
        getResourceBody
          >>= loadAndApplyTemplate "templates/default.html" contactCtx
          >>= relativizeUrls
          >>= removeIndexHtml
          
    peopleRules pandocCompiler'

    postRules pandocCompiler'

    match "index.html" $ do
      route $ setExtension "html"
      compile $ do
        posts <- fmap (take 5) . recentFirst =<< loadAll "posts/**"
        let indexCtx =
              constField "home-active" ""              `mappend`
              listField "posts" postCtx (return posts) `mappend`
              constField "title" "Home"                `mappend`
              defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls
          >>= removeIndexHtml

    match "templates/*" $ compile templateBodyCompiler

    -- http://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html
    let
      rss name render' =
        create [name] $ do
          route idRoute
          compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/**" "post-content"
            render' feedConfiguration feedCtx posts

    rss "rss.xml" renderRss
    rss "atom.xml" renderAtom

feedConfiguration ::
  FeedConfiguration
feedConfiguration =
  FeedConfiguration {
      feedTitle       = "Tony's Blog"
    , feedDescription = ""
    , feedAuthorName  = "Tony Morris"
    , feedAuthorEmail = "tmorris@tmorris.net"
    , feedRoot        = "https://blog.tmorris.net/"
    }
