{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Translate where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Traversable
import qualified JS.Ast as JS
import PatternMatching
import Wave.Ast
import Wave.Builtins hiding (builtins)
import Wave.Common

-- translate wave's AST to a subset of JS'AST
type TransState = Int

type TransReader = Builtins

type Translate a = (MonadState TransState a, MonadReader Builtins a)

translate :: (a -> StateT TransState (Reader Builtins) b) -> Builtins -> a -> b
translate tran builtins ast =
  let s = tran ast
      r = evalStateT s 0
      b = runReader r builtins
   in b

genVar :: Translate m => T.Text -> m Var
genVar var = do
  n <- get
  modify (+ 1)
  pure ("_" <> var <> "_" <> T.pack (show n))

--

translateFile :: Translate m => File () -> m JS.File
translateFile (File defs) = do
  defs' <- traverse translateDef termDefs
  pure $ JS.File $ map JS.SDef defs' ++ [JS.SExpr $ JS.EFunCall (JS.EVar "main") []]
  where
    termDefs = [d | (TermDef d) <- defs] -- datatype defs are not translated

translateDef :: Translate m => TermDef () -> m JS.Definition
translateDef = \case
  Variable var expr -> JS.Variable var <$> translateExpr expr
  Function var args body -> JS.Function var args <$> translateSub body

translateExpr :: Translate m => Expr () -> m JS.Expr
translateExpr = \case
  EAnnotated _ e -> translateExpr e
  ELit lit -> pure $ JS.ELit $ translateLit lit
  EVar var -> do
    builtinOp <- asks (M.lookup var)
    case builtinOp of
      Nothing -> pure $ JS.EVar var
      Just Builtin {bImpl} ->
        case bImpl of
          Fun f -> pure $ JS.ERaw f
          BinOp op ->
            pure $
              JS.EFun
                ["a", "b"]
                [JS.SRet $ JS.EBinOp op (JS.EVar "a") (JS.EVar "b")]
  EFun args body -> JS.EFun args <$> translateSub body
  EFunCall fun args -> do
    fun' <- translateExpr fun
    args' <- traverse translateExpr args
    pure $ JS.EFunCall fun' args'
  ERecord record -> JS.ERecord <$> traverse translateExpr record
  EFfi fun args -> JS.EFunCall (JS.EVar fun) <$> traverse translateExpr args
  EVariant (Variant "True" _) -> pure $ JS.ELit $ JS.LBool True
  EVariant (Variant "False" _) -> pure $ JS.ELit $ JS.LBool False
  EVariant (Variant kind value) -> do
    value' <- translateExpr value
    pure $
      JS.ERecord $
        M.fromList
          [ ("_kind", JS.ELit $ JS.LString kind),
            ("_value", value')
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

translatePatterns :: Translate m => JS.Expr -> [(Pattern, Expr ())] -> m JS.Sub
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
        { conditions = [JS.ELit $ JS.LBool True], -- always match
          matchers = [] -- but don't bind anything
        }
  PVar var ->
    pure
      PatResult
        { conditions = [JS.ELit $ JS.LBool True], -- always match
          matchers = [(var, expr)] -- and bind the var to the expression
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
  PVariant (Variant tag pat) -> do
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

translateSub :: Translate m => Sub () -> m JS.Sub
translateSub = traverse translateStatement

translateStatement :: Translate m => Statement () -> m JS.Statement
translateStatement = \case
  SExpr expr -> JS.SExpr <$> translateExpr expr
  SDef def -> JS.SDef <$> translateDef def

translateLit :: Lit -> JS.Lit
translateLit = \case
  LInt int -> JS.LInt int
  LFloat float -> JS.LFloat float
  LString str -> JS.LString str