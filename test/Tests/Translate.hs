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
        (translate translateStatement $ SExpr $ ELit $ LInt 1)
        (JS.SExpr $ JS.ELit $ JS.LInt 1)

    it "expr var" $
      shouldBe
        (translate translateStatement $ SExpr $ EVar "foobar")
        (JS.SExpr $ JS.EVar "foobar")

    it "expr fun" $
      shouldBe
        (translate translateStatement $ SExpr $ EFun ["x", "y"] [SExpr $ ELit $ LInt 1])
        (JS.SExpr $ JS.EFun ["x", "y"] [JS.SExpr $ JS.ELit $ JS.LInt 1])

    it "expr fun call" $
      shouldBe
        (translate translateStatement $ SExpr $ EFunCall (EVar "f") [EVar "x", EVar "y"])
        (JS.SExpr $ JS.EFunCall (JS.EVar "f") [JS.EVar "x", JS.EVar "y"])

    it "expr record" $
      shouldBe
        (translate translateStatement $ SExpr $ ERecord $ M.fromList [("a", ELit $ LInt 1)])
        (JS.SExpr $ JS.ERecord $ M.fromList [("a", JS.ELit $ JS.LInt 1)])

    it "expr ffi" $
      shouldBe
        (translate translateStatement $ SExpr $ EFfi "foobar" [EVar "x"])
        (JS.SExpr $ JS.EFunCall (JS.EVar "foobar") [JS.EVar "x"])

    it "expr variant" $
      shouldBe
        (translate translateStatement $ SExpr $ EVariant "tag" (ELit $ LInt 1))
        ( JS.SExpr $
            JS.ERecord $
              M.fromList
                [ ("_kind", JS.ELit (JS.LString "tag")),
                  ("_value", JS.ELit (JS.LInt 1))
                ]
        )

    it "expr case" $
      shouldBe
        ( translate translateStatement $
            SExpr $
              ECase
                (EVar "x")
                [ (PLit $ LInt 1, ELit $ LInt 10),
                  (PVar "y", ELit $ LInt 20)
                ]
        )
        ( JS.SExpr $
            JS.EFunCall
              ( JS.EFun
                  ["_case_0"]
                  [ JS.SIf
                      (JS.EAnd [JS.EEqual (JS.ELit $ JS.LInt 1) (JS.EVar "_case_0")])
                      [ JS.SRet
                          ( JS.EFunCall
                              (JS.EFun [] [JS.SRet (JS.ELit $ JS.LInt 10)])
                              []
                          )
                      ],
                    JS.SIf
                      (JS.EAnd [JS.ELit $ JS.LBool True])
                      [ JS.SRet
                          ( JS.EFunCall
                              (JS.EFun ["y"] [JS.SRet (JS.ELit $ JS.LInt 20)])
                              [JS.EVar "_case_0"]
                          )
                      ]
                  ]
              )
              [JS.EVar "x"]
        )

    it "expr record access" $
      shouldBe
        (translate translateStatement $ SExpr $ ERecordAccess (ERecord M.empty) "x")
        (JS.SExpr $ JS.ERecordAccess (JS.ERecord M.empty) "x")

    it "def var" $
      shouldBe
        (translate translateStatement $ SDef $ Variable "x" $ ELit $ LInt 1)
        (JS.SDef $ JS.Variable "x" $ JS.ELit $ JS.LInt 1)

    it "def fun" $
      shouldBe
        (translate translateStatement $ SDef $ Function "f" ["x", "y"] [SExpr $ ELit $ LInt 1])
        (JS.SDef $ JS.Function "f" ["x", "y"] [JS.SExpr $ JS.ELit $ JS.LInt 1])
