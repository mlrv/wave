module Wave.Ast where

import qualified Data.Map as M
import qualified Data.Text as T

type Var = T.Text

type Record a =
  M.Map Var a

data Lit
  = LInt Int
  | LFloat Float
  | LString T.Text
  deriving (Show, Eq)

data Statement
  = SExpr Expr
  | SDef Definition
  deriving (Show, Eq)

type Sub = [Statement]

data Pattern
  = PWildcard
  | PVar Var
  | PLit Lit
  | PRecord (Record Pattern)
  | PVariant T.Text Pattern
  deriving (Show, Eq)

data Expr
  = ELit Lit
  | EVar Var
  | EFun [Var] Sub
  | EFunCall Expr [Expr]
  | ERecord (Record Expr)
  | Effi T.Text [Expr]
  | EVariant T.Text Expr
  | ECase Expr [(Pattern, Expr)]
  deriving (Show, Eq)

data Definition
  = Variable Var Expr
  | Function Var [Var] Sub
  deriving (Show, Eq)

newtype File
  = File [Definition]
  deriving (Show, Eq)
