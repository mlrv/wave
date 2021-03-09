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
    expr

lit :: Spec
lit = do
  describe "Literals" $ do
    int
    float
    string
    bool

int :: Spec
int = do
  describe "int" $ do
    it "int simple" $
      shouldBe
        (ppLit' $ LInt 7)
        "7"

    it "int longer" $
      shouldBe
        (ppLit' $ LInt 123456)
        "123456"

    it "int negative" $
      shouldBe
        (ppLit' $ LInt $ -123456)
        "-123456"

    it "int zero" $
      shouldBe
        (ppLit' $ LInt 0)
        "0"

float :: Spec
float = do
  describe "float" $ do
    it "float simple" $
      shouldBe
        (ppLit' $ LFloat 7.12)
        "7.12"

    it "float longer" $
      shouldBe
        (ppLit' $ LFloat 1234.123)
        "1234.123"

    it "float negative" $
      shouldBe
        (ppLit' $ LFloat $ -1234.123)
        "-1234.123"

    it "float zero" $
      shouldBe
        (ppLit' $ LFloat 0.19)
        "0.19"

string :: Spec
string = do
  describe "string" $ do
    it "string simple" $
      shouldBe
        (ppLit' $ LString "wave")
        "wave"

    it "string with spaces" $
      shouldBe
        (ppLit' $ LString "wave wave   wave")
        "wave wave   wave"

    it "string escpaed" $
      shouldBe
        (ppLit' $ LString "\"hello world\"")
        "\"hello world\""

bool :: Spec
bool = do
  describe "bool" $ do
    it "true" $
      shouldBe
        (ppLit' $ LBool True)
        "true"

    it "false" $
      shouldBe
        (ppLit' $ LBool False)
        "false"

expr :: Spec
expr = do
  describe "Expressions" $ do
    it "lit" $
      shouldBe
        (ppExpr' $ ELit $ LInt 1)
        "1"

    it "var" $
      shouldBe
        (ppExpr' $ EVar "a")
        "a"

    it "function" $
      shouldBe
        (ppExpr' $ EFun ["a", "b"] [SRet (EVar "a")])
        "function(a, b) {\n  return a;\n}"

    it "function call with no arguments" $
      shouldBe
        (ppExpr' $ EFunCall (EVar "f") [])
        "f()"

    it "function call with one arguments" $
      shouldBe
        (ppExpr' $ EFunCall (EVar "f") [EVar "a"])
        "f(a)"

    it "function call with multiple arguments" $
      shouldBe
        (ppExpr' $ EFunCall (EVar "f") [EVar "a", EVar "b", EVar "c", EVar "d"])
        "f(a, b, c, d)"

    it "lamba function call" $
      shouldBe
        (ppExpr' $ EFunCall (EFun ["a", "b"] [SRet (EVar "a")]) [ELit $ LInt 1, ELit $ LInt 2])
        "(function(a, b) {\n  return a;\n})(1, 2)"

ppLit' :: Lit -> T.Text
ppLit' = pp ppLit

ppExpr' :: Expr -> T.Text
ppExpr' = pp ppExpr