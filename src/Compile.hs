module Compile where

import Translate
import qualified Data.Text as T
import JS.Pretty
import qualified Wave.Ast as Wave

compile :: Wave.File -> T.Text
compile = render . ppFile . translate translateFile