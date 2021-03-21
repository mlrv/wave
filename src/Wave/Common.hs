module Wave.Common where

import qualified Data.Map as M
import qualified Data.Text as T

type TypeVar = T.Text

type TypeCon = T.Text

type Label = T.Text

type Constr = T.Text

-- a record with keys of type String and values of type `a`, like
-- { x: 1, y: 2, z: 3 }
type Record a =
  M.Map Label a

-- a Data constructor
data Variant a
  = Variant Constr a
  deriving (Show, Eq)