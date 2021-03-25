-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.Hspec
import qualified Tests.Parser as Parser
import qualified Tests.Pretty as Pretty
import qualified Tests.Translate as Translate

main = do
  hspec spec

spec = do
  Pretty.spec
  Translate.spec
  Parser.spec