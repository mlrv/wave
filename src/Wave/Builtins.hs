{-# LANGUAGE OverloadedStrings #-}

module Wave.Builtins where

import qualified Data.Map as M
import qualified Data.Text as T
import Wave.Ast
import Wave.Common
import Wave.Types

type Builtins = M.Map Var Builtin

data Builtin = Builtin
  { bName :: Var,
    bType :: Type,
    bImpl :: Impl
  }

data Impl
  = Fun T.Text
  | BinOp T.Text

builtinFun :: Var -> Type -> T.Text -> (Var, Builtin)
builtinFun = builtin Fun

builtinBinOp :: Var -> Type -> T.Text -> (Var, Builtin)
builtinBinOp = builtin BinOp

builtin :: (T.Text -> Impl) -> Var -> Type -> T.Text -> (Var, Builtin)
builtin c n t i = (n, Builtin n t $ c i)

builtins :: Builtins
builtins =
  M.unions
    [ ints,
      strings,
      bools
    ]

ints :: Builtins
ints =
  M.fromList
    [ builtinBinOp "add" binOpInt "+",
      builtinBinOp "sub" binOpInt "-",
      builtinBinOp "mul" binOpInt "*",
      builtinBinOp "div" binOpInt "/",
      builtinFun "negate" opInt "function (x) { return 0 - x }"
    ]
  where
    binOpInt = TypeFun [tInt, tInt] tInt
    opInt = TypeFun [tInt] tInt

strings :: Builtins
strings =
  M.fromList
    [builtinBinOp "concat" binOpInt "+"]
  where
    binOpInt = TypeFun [tString, tString] tString

bools :: Builtins
bools =
  M.fromList
    [ builtinBinOp "and" binOpInt "&&",
      builtinBinOp "or" binOpInt "||",
      builtinFun "not" opInt "function (x) { return !x }"
    ]
  where
    binOpInt = TypeFun [tBool, tBool] tBool
    opInt = TypeFun [tBool] tBool

-- Builtin values
unit :: Expr
unit = ERecord M.empty

true :: Expr
true = EVariant $ Variant "True" unit

false :: Expr
false = EVariant $ Variant "False" unit

-- Builtin types
tUnit :: Type
tUnit = TypeRec []

tInt :: Type
tInt = TypeCon "Int"

tFloat :: Type
tFloat = TypeCon "Float"

tString :: Type
tString = TypeCon "String"

tBool :: Type
tBool = TypeCon "Bool"

-- Builtin datatypes
bool :: DataType
bool =
  DataType
    "Bool"
    []
    [ Variant "True" tUnit,
      Variant "False" tUnit
    ]

maybe :: DataType
maybe =
  DataType
    "Maybe"
    ["a"]
    [ Variant "Just" $ TypeVar "a",
      Variant "None" tUnit
    ]

