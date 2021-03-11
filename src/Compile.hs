module Compile where

import CodeGen
import qualified Data.Text as T
import JS.Pretty
import qualified Wave.Ast as Wave

compile :: Wave.File -> T.Text
compile = render . ppFile . translateFile