{-# LANGUAGE OverloadedStrings #-}

module Tests.Pretty where

import qualified Data.Map as M
import qualified Data.Text as T
import JS.Ast
import JS.Pretty
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pretty" $ do
    lit

lit :: Spec
lit = do
  describe "Literals" $ do
    int

int :: Spec
int = do
  describe "int" $ do
    it "int simple" $
      shouldBe
        (ppLit' $ LInt 7)
        "7"

ppLit' :: Lit -> T.Text
ppLit' = pp ppLit