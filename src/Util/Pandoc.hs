{-# LANGUAGE NoImplicitPrelude #-}

module Util.Pandoc(
  pandocCompiler'
) where

import Data.Function(($))
import Data.String(String)
import qualified Data.Set as Set(delete)
import Hakyll(Compiler, Item, defaultHakyllWriterOptions, defaultHakyllReaderOptions, pandocCompilerWith)
import Text.Pandoc(WriterOptions, writerExtensions, Extension(Ext_literate_haskell))

pandocCompiler' ::
  Compiler (Item String)
pandocCompiler' =
  let writerOptions ::
        WriterOptions
      writerOptions =
        let
          d = defaultHakyllWriterOptions
        in
          d
          { writerExtensions =
              Set.delete Ext_literate_haskell $ writerExtensions d
          }
  in  pandocCompilerWith
        defaultHakyllReaderOptions
        writerOptions
