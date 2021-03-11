module Wave.Ast where

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
  | LFloat Float
  | LString T.Text
  deriving (Show)

--
data Statement
  = SExpr Expr
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
  | Effi T.Text [Expr]
  deriving (Show)

--
data Definition
  = Variable Var Expr
  | Function Var [Var] Sub
  deriving (Show)

--
newtype File
  = File [Definition]
  deriving (Show)
