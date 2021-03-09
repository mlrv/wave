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

ppLit' :: Lit -> T.Text
ppLit' = pp ppLit