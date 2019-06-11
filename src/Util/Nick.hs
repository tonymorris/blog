{-# LANGUAGE NoImplicitPrelude #-}

module Util.Nick(
  mkNickCtx
) where

import Control.Monad(return)
import Data.Function(($))
import Data.Maybe(Maybe(Just, Nothing), maybe)
import Data.Monoid(mempty)
import Data.String(String)
import Hakyll(Compiler, Pattern, Context, getUnderlying, capture, constField)

mkNickCtx ::
  Pattern
  -> Compiler (Context String)
mkNickCtx x = do
  identifier <- getUnderlying
  let
    mIdent =
      case capture x identifier of
        Just [ident] -> Just ident
        _ -> Nothing
  return $ maybe mempty (constField "nick") mIdent
