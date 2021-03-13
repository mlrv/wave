{-# LANGUAGE OverloadedStrings #-}

module Translate where

import qualified Data.Map as M
import qualified JS.Ast as JS
import Wave.Ast

translateFile :: File -> JS.File
translateFile (File defs) = JS.File $ map (JS.SDef . translateDef) defs

translateDef :: Definition -> JS.Definition
translateDef = \case
  Variable var expr -> JS.Variable var (translateExpr expr)
  Function var args body -> JS.Function var args (translateSub body)

translateExpr :: Expr -> JS.Expr
translateExpr = \case
  ELit lit -> JS.ELit (translateLit lit)
  EVar var -> JS.EVar var
  EFun args body -> JS.EFun args (translateSub body)
  EFunCall fun args -> JS.EFunCall (translateExpr fun) (map translateExpr args)
  ERecord record -> JS.ERecord (fmap translateExpr record)
  Effi fun args -> JS.EFunCall (JS.EVar fun) (map translateExpr args)
  EVariant kind value ->
    JS.ERecord $
      M.fromList
        [ ("_kind", JS.ELit $ JS.LString kind),
          ("_value", translateExpr value)
        ]
  -- ECase {} -> undefined

translateLit :: Lit -> JS.Lit
translateLit = \case
  LInt int -> JS.LInt int
  LFloat float -> JS.LFloat float
  LString str -> JS.LString str

translateSub :: Sub -> JS.Sub
translateSub = map translateStatement

translateStatement :: Statement -> JS.Statement
translateStatement = \case
  SExpr expr -> JS.SExpr (translateExpr expr)
  SDef def -> JS.SDef (translateDef def)
