module Compile where

import qualified Data.Text as T
import JS.Pretty
import Translate
import qualified Wave.Ast as Wave
import Wave.Builtins

compile :: Wave.File () -> T.Text
compile = render . ppFile . translate translateFile builtins