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
    record

record :: Spec
record = do
  describe "Records" $ do
    it "empty" $
      shouldBe
        (ppRecord' M.empty)
        "{}"
    it "one element" $
      shouldBe
        (ppRecord' $ M.fromList [("a", ELit $ LInt 1)])
        "{\"a\": 1}"
    it "many elements" $
      shouldBe
        ( ppRecord' $
            M.fromList
              [ ("a", ELit $ LInt 1),
                ("b", ELit $ LBool True),
                ("c", ELit $ LString "abc")
              ]
        )
        "{\"a\": 1, \"b\": true, \"c\": \"abc\"}"
    it "nested" $
      shouldBe
        ( ppRecord' $
            M.fromList
              [ ("a", ELit $ LInt 1),
                ( "b",
                  ERecord $
                    M.fromList
                      [ ("c", ELit $ LFloat 2.3),
                        ("d", EVar "foo")
                      ]
                )
              ]
        )
        "{\"a\": 1, \"b\": {\"c\": 2.3, \"d\": foo}}"

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
        "\"wave\""

    it "string with spaces" $
      shouldBe
        (ppLit' $ LString "wave wave   wave")
        "\"wave wave   wave\""

    it "string escpaed" $
      shouldBe
        (ppLit' $ LString "\"hello world\"")
        "\"\"hello world\"\""

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

    it "function call with one argument" $
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

ppRecord' :: Record Expr -> T.Text
ppRecord' = pp $ ppRecord ppExpr

ppLit' :: Lit -> T.Text
ppLit' = pp ppLit

ppExpr' :: Expr -> T.Text
ppExpr' = pp ppExpr