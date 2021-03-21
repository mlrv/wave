module Wave.Types where

import Wave.Common ( Label, TypeCon, TypeVar )

data Type
  = TypeVar TypeVar -- type variable
  | TypeCon TypeCon -- type constructor
  | TypeApp Type Type -- type application
  | TypeFun [Type] Type -- type of a function
  | TypeRec [(Label, Type)] Type -- type of a record
