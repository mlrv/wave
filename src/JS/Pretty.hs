{-# LANGUAGE OverloadedStrings #-}

module JS.Pretty where

import qualified Data.Map as M
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import JS.Ast

ppRecord :: Pretty a => Record a -> Doc a
ppRecord r =
  encloseSep "{" "}" ", " $
    fmap
      (\(k, v) -> surround ": " (pretty $ show k) v)
      (M.toList (fmap pretty r))

ppLit :: Lit -> Doc a
ppLit = \case
  LInt int -> pretty int
  LFloat float -> pretty float
  LString str -> pretty str
  LBool True -> "true"
  LBool False -> "false"
