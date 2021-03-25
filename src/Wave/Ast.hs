module Wave.Ast where

-- wave's AST

import qualified Data.Text as T
import Wave.Common
import Wave.Types

-- the name of a variable
type Var = T.Text

-- a literal, either an Integer, a Float, or a String
data Lit
  = LInt Int
  | LFloat Float
  | LString T.Text
  deriving (Show, Eq)

-- an expression that evaluates to a value
data Expr a
  = EAnnotated a (Expr a)
  | ELit Lit
  | EVar Var
  | EFun [Var] (Sub a)
  | EFunCall (Expr a) [Expr a]
  | ERecord (Record (Expr a))
  | EFfi T.Text [Expr a] -- foreign function interface call
  | EVariant (Variant (Expr a)) -- equivalent to data constructors
  | ECase (Expr a) [(Pattern, Expr a)]
  | ERecordAccess (Expr a) Label
  deriving (Show, Eq)

-- a datatype
data DataType
  = DataType Constr [TypeVar] [Variant Type]
  deriving (Show, Eq)

-- a datatype definition or term definition
data Definition a
  = TypeDef DataType
  | TermDef (TermDef a)
  deriving (Show, Eq)

-- a term definition, either a variable or a function
data TermDef a
  = Variable Var (Expr a)
  | Function Var [Var] (Sub a)
  deriving (Show, Eq)

-- a statement, either an expression or a term definition
data Statement a
  = SExpr (Expr a)
  | SDef (TermDef a)
  deriving (Show, Eq)

-- a series of statements
type Sub a = [Statement a]

-- a pattern
data Pattern
  = PWildcard -- match all
  | PVar Var -- match all and bind the expression to a variable
  | PLit Lit
  | PRecord (Record Pattern)
  | PVariant (Variant Pattern)
  deriving (Show, Eq)

-- a wave's source file, a collection of definitions
newtype File a
  = File [Definition a]
  deriving (Show, Eq)
