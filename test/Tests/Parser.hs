{-# LANGUAGE OverloadedStrings #-}

module Tests.Parser where

import qualified Data.Map as M
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck
import Wave.Ast
import Wave.Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parser" $ do
    lits

lits :: Spec
lits = do
  describe "literals" $ do
    numbers
    strings

numbers :: Spec
numbers = do
  describe "numbers" $ do
    it "zero" $
      shouldBe
        (testParser parseLit "0")
        (Right $ LInt 0)
    it "postive" $
      shouldBe
        (testParser parseLit "998877")
        (Right $ LInt 998877)
    it "negative" $
      shouldBe
        (testParser parseLit "-230")
        (Right $ LInt $ -230)
    it "float positive" $
      shouldBe
        (testParser parseLit "31.99")
        (Right $ LFloat 31.99)
    it "float negative" $
      shouldBe
        (testParser parseLit "-31.99")
        (Right $ LFloat $ -31.99)

strings :: Spec
strings = do
  describe "strings" $ do
    it "empty" $
      shouldBe
        (testParser parseLit "\"\"")
        (Right $ LString "")
    it "non empty" $
      shouldBe
        (testParser parseLit "\"wave\"")
        (Right $ LString "wave")
    it "with spaces" $
      shouldBe
        (testParser parseLit "\"wa ve\"")
        (Right $ LString "wa ve")
    it "with single quotes" $
      shouldBe
        (testParser parseLit "\"wave's wave\"")
        (Right $ LString "wave's wave")
    it "with newlines" $
      shouldBe
        (testParser parseLit "\"wave\nwave\"")
        (Right $ LString "wave\nwave")

testParser :: Parser a -> T.Text -> Either T.Text a
testParser p = runParser p "test"