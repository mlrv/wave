{-# LANGUAGE OverloadedStrings #-}

module JS.Pretty where

import qualified Data.Map as M
import qualified Data.Text as T
import JS.Ast
import Prettyprinter
import Prettyprinter.Render.Text

pp :: (a -> Doc a) -> a -> T.Text
pp f = render . f

render :: Doc a -> T.Text
render = renderStrict . layoutPretty defaultLayoutOptions

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
