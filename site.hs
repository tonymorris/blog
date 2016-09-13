{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Prelude(String, FilePath, IO, Monad(..), Functor(..), take, all, (=<<), (++), (.), ($))
import Data.List(drop)
import Data.Monoid(mappend, mconcat)
import Data.Maybe(fromMaybe)
import Text.Pandoc(WriterOptions, HTMLMathMethod(..), writerHTMLMathMethod)
import System.FilePath(joinPath, splitDirectories, dropExtension, combine)
import Control.Lens(_last, over)
import Data.Char(isDigit)
import Hakyll

postsPattern ::
  Pattern
postsPattern =
  "posts/*" .&&. complement "posts/index.html"

main ::
  IO ()
main =
  hakyllWith blogConfiguration $ do
    match "static/*" $ do
        route (customRoute (joinPath . drop 1 . splitDirectories . toFilePath))
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Build tags
    tags <- buildTags postsPattern (fromCapture "tags/*.html")

    match (fromList ["contact.markdown", "404.markdown"]) $ do
        route (customRoute directoryIdentifier)
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    match postsPattern $ do
        route (customRoute directoryIdentifier)
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= saveSnapshot "content"
            -- >>= return . fmap demoteHeaders
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= loadAndApplyTemplate "templates/disqus.html"  (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    create ["posts/index.html"] $ do
        route idRoute
        compile $ do
            list <- postList tags postsPattern recentFirst
            let archiveCtx =
                    constField "posts" list                    `mappend`
                    constField "title" "Posts"                 `mappend`
                    postCtx tags

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"   archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag
        route (customRoute directoryIdentifier)
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html"
                        (constField "title" title   `mappend`
                            constField "posts" list `mappend`
                            constField "tag" tag `mappend`
                            field "tags" (\_ -> renderTagList tags) `mappend`
                            defaultCtx)
                >>= loadAndApplyTemplate "templates/default.html" defaultCtx
                >>= relativizeUrls
        version "atom" $ do
            route $ setExtension "xml"
            compile $ loadAllSnapshots pattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom (feedConfiguration title) feedCtx

    match "index.html" $ do
        route idRoute
        compile $ do
            list <- postList tags postsPattern $ fmap (take 5) . recentFirst
            let indexCtx = constField "posts" list          `mappend`
                    field "tags" (\_ -> renderTagList tags) `mappend`
                    defaultCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" defaultCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    -- Render RSS feed
    create ["atom.xml"] $ do
        route idRoute
        compile $
            loadAllSnapshots postsPattern "content"
                >>= recentFirst
                >>= renderAtom (feedConfiguration "All posts") feedCtx
    
colourField ::
  String
  -> String
  -> Context a
colourField name defaultC = field name $ \i -> do
  metadata <- getMetadata (itemIdentifier i)
  return . fromMaybe defaultC . lookupString "colour" $ metadata

defaultColour ::
  String
defaultColour =
  "light cyan"

postCtx ::
  Tags
  -> Context String
postCtx tags =
  mconcat
    [ 
      tagsField  "tags"  tags
    , dateField  "date"  "%B %e, %Y"
    , defaultCtx
    ]

defaultCtx ::
  Context String
defaultCtx =
   colourField "colour" defaultColour `mappend` defaultContext

postList ::
  Tags
  -> Pattern
  -> ([Item String] -> Compiler [Item String])
  -> Compiler String
postList tags pattern sortFilter = do
  posts   <- sortFilter =<< loadAll pattern
  itemTpl <- loadBody "templates/post-item.html"
  applyTemplateList itemTpl (postCtx tags) posts

feedCtx ::
  Context String
feedCtx =
  mconcat
    [ 
      bodyField "description"
    , defaultContext
    ]

feedConfiguration ::
  String
  -> FeedConfiguration
feedConfiguration title =
  FeedConfiguration
    { 
      feedTitle       = "λ Tony's blog λ — " ++ title
    , feedDescription = "The weblog of Tony Morris"
    , feedAuthorName  = "Tony Morris"
    , feedAuthorEmail = "blog@tmorris.net"
    , feedRoot        = "http://blog.tmorris.net"
    }

pandocOptions ::
  WriterOptions
pandocOptions =
  defaultHakyllWriterOptions {
    writerHTMLMathMethod = MathJax ""
  }

blogConfiguration ::
  Configuration
blogConfiguration =
  defaultConfiguration { deployCommand = "cp -r _site/* ../" }

directoryIdentifier ::
  Identifier
  -> FilePath
directoryIdentifier =
  let dropDate (y1:y2:y3:y4:'-':m1:m2:'-':d1:d2:_:r) | all isDigit [y1,y2,y3,y4,m1,m2,d1,d2] = r
      dropDate x = x
  in (`combine` "index.html") . joinPath . over _last dropDate . splitDirectories . dropExtension . toFilePath
