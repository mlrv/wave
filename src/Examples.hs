{-# LANGUAGE OverloadedStrings #-}

module Examples where

import qualified Data.Map as M
import Wave.Ast
import Wave.Common

lit :: File ()
lit =
  exprToFile $
    ELit $ LInt 7

variant :: File ()
variant =
  exprToFile $
    EVariant $ Variant "Foo" (ERecord $ M.fromList [("x", ELit $ LString "wave")])

patmatch1 :: File ()
patmatch1 =
  exprToFile $
    ECase
      (ELit $ LInt 0)
      [ (PWildcard, ELit $ LInt 1)
      ]

patmatch2 :: File ()
patmatch2 =
  exprToFile $
    ECase
      (ELit $ LInt 0)
      [ (PLit (LInt 1), ELit $ LInt 1),
        (PLit (LInt 0), ELit $ LInt 0)
      ]

patmatch3 :: File ()
patmatch3 =
  exprToFile $
    ECase
      (ELit $ LInt 17)
      [ (PLit (LInt 1), ELit $ LInt 1),
        (PLit (LInt 0), ELit $ LInt 0),
        (PVar "v", EVar "v")
      ]

patmatch4 :: File ()
patmatch4 =
  exprToFile $
    ECase
      ( EVariant $
          Variant "Nil" $
            ERecord $
              M.fromList
                [ ("head", ELit $ LInt 0),
                  ("tail", ERecord mempty)
                ]
      )
      [ (PVariant $ Variant "Nil" (PVar "obj"), ERecordAccess (EVar "obj") "head")
      ]

patmatch5 :: File ()
patmatch5 =
  exprToFile $
    ECase
      ( EVariant $
          Variant "Nil" $
            ERecord $
              M.fromList
                [ ("head", ELit $ LInt 0),
                  ("tail", ERecord mempty)
                ]
      )
      [ ( PVariant $
            Variant
              "Nil"
              ( PRecord $
                  M.fromList
                    [ ("head", PVar "head"),
                      ("tail", PRecord mempty)
                    ]
              ),
          EVar "head"
        )
      ]

exprToFile :: Expr () -> File ()
exprToFile e =
  File
    [ TermDef $
        Function
          "main"
          []
          [SExpr $ EFfi "console.log" [e]]
    ]