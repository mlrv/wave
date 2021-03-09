module JS.Ast where

import qualified Data.Map as M
import qualified Data.Text as T

--
type Var = T.Text

--
type Record a =
  M.Map Var a

--
data Lit
  = LInt Int
  | LBool Bool
  | LFloat Float
  | LString T.Text
  deriving (Show)

--
data Statement
  = SExpr Expr
  | SRet Expr
  | SDef Definition
  deriving (Show)

--
type Sub = [Statement]

--
data Expr
  = ELit Lit
  | EVar Var
  | EFun [Var] Sub
  | EFunCall Expr [Expr]
  | ERecord (Record Expr)
  deriving (Show)

--
data Definition
  = Variable Var Expr
  | Function Var [Var] Sub
  deriving (Show)

--
data File
  = File [Statement]
  deriving (Show)
