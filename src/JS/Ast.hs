module JS.Ast where

import qualified Data.Map as M
import qualified Data.Text as T

type Var = T.Text

type Record a =
  M.Map Var a

data Lit
  = LInt Int
  | LBool Bool
  | LFloat Float
  | LString T.Text
  deriving (Show, Eq)

data Statement
  = SExpr Expr
  | SRet Expr
  | SDef Definition
  | SIf Expr Sub
  deriving (Show, Eq)

type Sub = [Statement]

data Expr
  = ELit Lit
  | EVar Var
  | EFun [Var] Sub
  | EFunCall Expr [Expr]
  | ERecord (Record Expr)
  | EAnd [Expr]
  | EEqual Expr Expr
  | ERecordAccess Expr Label
  | EBinOp T.Text Expr Expr
  | ERaw T.Text
  deriving (Show, Eq)

type Label = T.Text 

data Definition
  = Variable Var Expr
  | Function Var [Var] Sub
  deriving (Show, Eq)

newtype File
  = File [Statement]
  deriving (Show, Eq)
