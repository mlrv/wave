{-# LANGUAGE OverloadedStrings #-}

module JS.Pretty where

import qualified Data.Map as M
import qualified Data.Text as T
import JS.Ast
import Prettyprinter
import Prettyprinter.Render.Text

pp :: (a -> Doc ann) -> a -> T.Text
pp f = render . f

render :: Doc a -> T.Text
render = renderStrict . layoutPretty defaultLayoutOptions

ppFile :: File -> Doc a
ppFile (File file) = vsep $ ppStatement <$> file

ppRecord :: (a -> Doc ann) -> Record a -> Doc ann
ppRecord p r =
  encloseSep "{" "}" ", " $
    fmap
      (\(k, v) -> surround ": " (pretty $ show k) v)
      (M.toList (fmap p r))

ppLit :: Lit -> Doc a
ppLit = \case
  LInt int -> pretty int
  LFloat float -> pretty float
  LString str -> "\"" <> pretty str <> "\""
  LBool True -> "true"
  LBool False -> "false"

ppSub :: Sub -> Doc a
ppSub sub = vsep (ppStatement <$> sub)

ppDef :: Definition -> Doc a
ppDef = \case
  Variable name expr -> "const" <+> pretty name <+> "=" <+> ppExpr expr <> ";"
  Function name args body ->
    let arguments = concatWith (surround ", ") (pretty <$> args)
     in "const" <+> pretty name <+> "="
          <+> vsep
            [ "function" <> "(" <> arguments <> ")" <+> "{",
              indent 2 $ ppSub body <> ";",
              "}"
            ]

ppStatement :: Statement -> Doc a
ppStatement = \case
  SExpr expr -> ppExpr expr
  SRet expr -> "return" <+> ppExpr expr
  SDef def -> ppDef def
  SIf cond sub ->
    vsep
      [ "if" <+> parens (ppExpr cond) <+> "{",
        indent 2 $ ppSub sub,
        "}"
      ]

ppExpr :: Expr -> Doc a
ppExpr = \case
  ELit lit -> ppLit lit
  EVar var -> pretty var
  EFun args body ->
    let arguments = concatWith (surround ", ") (pretty <$> args)
     in vsep
          [ "function" <> "(" <> arguments <> ")" <+> "{",
            indent 2 $ ppSub body <> ";",
            "}"
          ]
  EFunCall fun args ->
    let encloseIfNotSimple = if isSimpleExpr fun then id else parens
        arguments = concatWith (surround ", ") (ppExpr <$> args)
     in encloseIfNotSimple (ppExpr fun) <> "(" <> arguments <> ")"
  ERecord record -> ppRecord ppExpr record
  EAnd exprs -> concatWith (surround " && ") (ppExpr <$> exprs)
  EEqual a b -> ppExpr $ EBinOp "===" a b
  ERecordAccess expr label ->
    (if isSimpleExpr expr then id else parens) (ppExpr expr)
      <> "."
      <> pretty label
  EBinOp op a b -> ppExpr a <+> pretty op <+> ppExpr b
  ERaw raw -> pretty raw

-- need surrounding parens?
isSimpleExpr :: Expr -> Bool
isSimpleExpr = \case
  ERecord {} -> False
  EFun {} -> False
  EEqual {} -> False
  EBinOp {} -> False
  ERaw {} -> False
  _ -> True