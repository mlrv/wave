{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Translate where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Traversable
import qualified JS.Ast as JS
import PatternMatching
import Wave.Ast

-- translate wave's AST to a subset of JS'AST
type Translate a = MonadState Int a

translate :: (a -> State Int b) -> a -> b
translate tran ast = evalState (tran ast) 0

genVar :: Translate m => T.Text -> m Var
genVar var = do
  n <- get
  modify (+ 1)
  pure ("_" <> var <> "_" <> T.pack (show n))

--

translateFile :: Translate m => File -> m JS.File
translateFile (File defs) = do
  defs' <- traverse translateDef defs
  pure $ JS.File $ map JS.SDef defs'

translateDef :: Translate m => Definition -> m JS.Definition
translateDef = \case
  Variable var expr -> JS.Variable var <$> translateExpr expr
  Function var args body -> JS.Function var args <$> translateSub body

translateExpr :: Translate m => Expr -> m JS.Expr
translateExpr = \case
  ELit lit -> pure $ JS.ELit $ translateLit lit
  EVar var -> pure $ JS.EVar var
  EFun args body -> JS.EFun args <$> translateSub body
  EFunCall fun args -> do
    fun' <- translateExpr fun
    args' <- traverse translateExpr args
    pure $ JS.EFunCall fun' args'
  ERecord record -> JS.ERecord <$> traverse translateExpr record
  EFfi fun args -> JS.EFunCall (JS.EVar fun) <$> traverse translateExpr args
  EVariant kind value -> do
    value' <- translateExpr value
    pure $
      JS.ERecord $
        M.fromList
          [ ("_kind", JS.ELit $ JS.LString kind),
            ("_value", value') -- extract pattern stuff in
          ]
  ECase expr patterns -> do
    expr' <- translateExpr expr
    var <- genVar "case"
    patterns' <- translatePatterns (JS.EVar var) patterns
    pure $
      JS.EFunCall
        (JS.EFun [var] patterns')
        [expr']
  ERecordAccess expr label -> do
    expr' <- translateExpr expr
    pure $
      JS.ERecordAccess expr' label

translatePatterns :: Translate m => JS.Expr -> [(Pattern, Expr)] -> m JS.Sub
translatePatterns var = traverse $ \(pat, expr) -> do
  result' <- translateExpr expr
  PatResult conds matchers' <- translatePattern var pat
  let (matchersV, matchersE) = unzip matchers'
  pure $
    JS.SIf
      (JS.EAnd conds)
      [ JS.SRet $
          JS.EFunCall
            (JS.EFun matchersV [JS.SRet result'])
            matchersE
      ]

translatePattern :: Translate m => JS.Expr -> Pattern -> m PatResult
translatePattern expr = \case
  PWildcard ->
    pure
      PatResult
        { conditions = [JS.ELit $ JS.LBool True],
          matchers = []
        }
  PVar var ->
    pure
      PatResult
        { conditions = [JS.ELit $ JS.LBool True],
          matchers = [(var, expr)]
        }
  PLit lit ->
    pure
      PatResult
        { conditions = [JS.EEqual (JS.ELit $ translateLit lit) expr],
          matchers = []
        }
  PRecord (M.toList -> fields) -> do
    fmap mconcat $
      for fields $ \(field, pat) ->
        translatePattern (JS.ERecordAccess expr field) pat
  PVariant tag pat -> do
    pat' <- translatePattern (JS.ERecordAccess expr "_value") pat
    pure
      PatResult
        { conditions =
            JS.EEqual
              (JS.ELit $ JS.LString tag)
              (JS.ERecordAccess expr "_kind") :
            conditions pat',
          matchers = matchers pat'
        }

translateSub :: Translate m => Sub -> m JS.Sub
translateSub = traverse translateStatement

translateStatement :: Translate m => Statement -> m JS.Statement
translateStatement = \case
  SExpr expr -> JS.SExpr <$> translateExpr expr
  SDef def -> JS.SDef <$> translateDef def

translateLit :: Lit -> JS.Lit
translateLit = \case
  LInt int -> JS.LInt int
  LFloat float -> JS.LFloat float
  LString str -> JS.LString str