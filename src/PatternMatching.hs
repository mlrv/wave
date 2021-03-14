module PatternMatching where

import qualified JS.Ast as JS
import Wave.Ast

-- We want to translate `pattern -> result` to something like
--
-- if (conditions) {
--   return function (matchersVar*) {
--     return result
--   }(matchersExpr*)
-- }

data PatResult = PatResult
  { conditions :: [JS.Expr],
    matchers :: [(Var, JS.Expr)]
  }

instance Semigroup PatResult where
  (<>) (PatResult c1 m1) (PatResult c2 m2) =
    PatResult (c1 <> c2) (m1 <> m2)

instance Monoid PatResult where
  mempty = PatResult [] []