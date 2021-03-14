module Wave.Ast where

-- wave's AST

import qualified Data.Map as M
import qualified Data.Text as T

-- the name of a variable
type Var = T.Text

-- a literal, either an Integer, a Float, or a String
data Lit
  = LInt Int
  | LFloat Float
  | LString T.Text
  deriving (Show, Eq)

-- a simple text label
type Label = T.Text

-- a record with keys of type String and values of type `a`, like
-- { "x": 1, "y": 2, "z": 3 }
type Record a =
  M.Map Label a

-- an expression that evaluates to a value
data Expr
  = ELit Lit
  | EVar Var
  | EFun [Var] Sub
  | EFunCall Expr [Expr]
  | ERecord (Record Expr)
  | EFfi T.Text [Expr] -- foreign function interface call
  | EVariant T.Text Expr -- equivalent to data constructors
  | ECase Expr [(Pattern, Expr)]
  | ERecordAccess Expr Label
  deriving (Show, Eq)

-- a definition, either a variable or a function
data Definition
  = Variable Var Expr
  | Function Var [Var] Sub
  deriving (Show, Eq)

-- a statement, either an expression or a definition
data Statement
  = SExpr Expr
  | SDef Definition
  deriving (Show, Eq)

-- a series of statements
type Sub = [Statement]

-- a pattern
data Pattern
  = PWildcard -- match all
  | PVar Var -- match all and bind the expression to a variable
  | PLit Lit
  | PRecord (Record Pattern)
  | PVariant T.Text Pattern
  deriving (Show, Eq)

-- a wave's source file, a collection of definitions
newtype File
  = File [Definition]
  deriving (Show, Eq)
