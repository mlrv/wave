{-# LANGUAGE OverloadedStrings #-}

module Tests.Translate where

import Compile
import qualified Data.Map as M
import qualified Data.Text.IO as T
import qualified JS.Ast as JS
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)
import Test.Hspec
import Test.QuickCheck hiding (variant)
import Translate
import Wave.Ast

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  runIO $ createDirectoryIfMissing False testDir
  describe "translate" $ do
    lit
    record
    variant
    recordAccess
    patternMatch

lit :: Spec
lit = do
  describe "Lit" $ do
    it "int" $
      check
        "lit int"
        (exprToFile $ ELit $ LInt 1)
        "1"
    it "float" $
      check
        "lit float"
        (exprToFile $ ELit $ LFloat 1.23)
        "1.23"
    it "string" $
      check
        "lit string"
        (exprToFile $ ELit $ LString "wave")
        "wave"

record :: Spec
record = do
  describe "Record" $ do
    it "simple" $
      check
        "record simple"
        ( exprToFile $
            ERecord $
              M.fromList
                [("x", ELit $ LInt 1), ("y", ELit $ LString "wave")]
        )
        "{ x: 1, y: 'wave' }"
    it "nested" $
      check
        "record nested"
        ( exprToFile $
            ERecord $
              M.fromList
                [ ("a", ELit $ LInt 1),
                  ( "b",
                    ERecord $
                      M.fromList
                        [ ( "c",
                            ERecord $
                              M.fromList
                                [ ("d", ELit $ LInt 1)
                                ]
                          )
                        ]
                  )
                ]
        )
        "{ a: 1, b: { c: { d: 1 } } }"

variant :: Spec
variant = do
  describe "Variant" $ do
    it "simple" $
      check
        "variant simple"
        (exprToFile $ EVariant "Foo" $ ELit $ LInt 1)
        "{ _kind: 'Foo', _value: 1 }"
    it "complex" $
      check
        "variant complex"
        (exprToFile $ EVariant "Foo" $ EVariant "Bar" $ ELit $ LString "wave")
        "{ _kind: 'Foo', _value: { _kind: 'Bar', _value: 'wave' } }"

recordAccess :: Spec
recordAccess = do
  describe "Record Access" $ do
    it "empty" $
      check
        "record access empty"
        (exprToFile $ ERecordAccess (ERecord M.empty) "x")
        "undefined"
    it "simple" $
      check
        "record access simple"
        (exprToFile $ ERecordAccess (ERecord $ M.fromList [("x", ELit $ LInt 1)]) "x")
        "1"
    it "complex" $
      check
        "record access complex"
        ( exprToFile $
            ERecordAccess
              ( ERecordAccess
                  ( ERecord $
                      M.fromList
                        [ ("x", ERecord $ M.fromList [("y", ELit $ LInt 1)])
                        ]
                  )
                  "x"
              )
              "y"
        )
        "1"

patternMatch :: Spec
patternMatch = do
  describe "Pattern Match" $ do
    it "wildcard" $
      check
        "patternMatch wildcard"
        ( exprToFile $
            ECase
              (ELit $ LInt 1)
              [(PWildcard, ELit $ LInt 2)]
        )
        "2"
    it "case int" $
      check
        "patternMatch case int"
        ( exprToFile $
            ECase
              (ELit $ LInt 1)
              [ (PLit $ LInt 2, ELit $ LInt 2),
                (PLit $ LInt 1, ELit $ LInt 1)
              ]
        )
        "1"
    it "case var" $
      check
        "patternMatch case var"
        ( exprToFile $
            ECase
              (ELit $ LInt 1)
              [ (PLit $ LInt 2, ELit $ LInt 2),
                (PLit $ LInt 3, ELit $ LInt 3),
                (PVar "x", EVar "x")
              ]
        )
        "1"
    it "case variant key" $
      check
        "patternMatch case variant key"
        ( exprToFile $
            ECase
              ( EVariant "Foo" $
                  ERecord $
                    M.fromList
                      [ ("x", ELit $ LInt 1),
                        ("y", ELit $ LString "wave")
                      ]
              )
              [(PVariant "Foo" (PVar "foo"), ERecordAccess (EVar "foo") "y")]
        )
        "wave"
    it "case variant record" $
      check
        "patternMatch case variant record"
        ( exprToFile $
            ECase
              ( EVariant "Foo" $
                  ERecord $
                    M.fromList
                      [ ("x", ELit $ LInt 1),
                        ("y", ELit $ LString "wave")
                      ]
              )
              [ ( PVariant
                    "Foo"
                    ( PRecord $
                        M.fromList
                          [ ("x", PVar "x_match"),
                            ("y", PLit $ LString "wave")
                          ]
                    ),
                  EVar "x_match"
                )
              ]
        )
        "1"

-- utilities
check :: FilePath -> File -> String -> IO ()
check path file expected =
  let fileName = testDir <> path <> ".wave"
   in do
        T.writeFile fileName $ compile file
        result <- readProcess "node" [fileName] ""
        result `shouldBe` expected <> "\n"

testDir = "/tmp/test"

exprToFile :: Expr -> File
exprToFile expr =
  File
    [ Function
        "main"
        []
        [ SExpr $ EFfi "console.log" [expr]
        ]
    ]
