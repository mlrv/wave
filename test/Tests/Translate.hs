{-# LANGUAGE OverloadedStrings #-}

module Tests.Translate where

import qualified Data.Map as M
import qualified Data.Text as Ty
import qualified JS.Ast as JS
import Test.Hspec
import Test.QuickCheck
import Translate
import Wave.Ast

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "translate" $ do
    statement

statement :: Spec
statement = do
  describe "Statements" $ do
    it "expr lit" $
      shouldBe
        (translateStatement $ SExpr $ ELit $ LInt 1)
        (JS.SExpr $ JS.ELit $ JS.LInt 1)
    it "expr var" $
      shouldBe
        (translateStatement $ SExpr $ EVar "foobar")
        (JS.SExpr $ JS.EVar "foobar")
    it "expr fun" $
      shouldBe
        (translateStatement $ SExpr $ EFun ["x", "y"] [SExpr $ ELit $ LInt 1])
        (JS.SExpr $ JS.EFun ["x", "y"] [JS.SExpr $ JS.ELit $ JS.LInt 1])

    it "def var" $
      shouldBe
        (translateStatement $ SDef $ Variable "x" $ ELit $ LInt 1)
        (JS.SDef $ JS.Variable "x" $ JS.ELit $ JS.LInt 1)
    it "def fun" $
      shouldBe
        (translateStatement $ SDef $ Function "f" ["x", "y"] [SExpr $ ELit $ LInt 1])
        (JS.SDef $ JS.Function "f" ["x", "y"] [JS.SExpr $ JS.ELit $ JS.LInt 1])
