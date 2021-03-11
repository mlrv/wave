module Main where

main :: IO ()
main = print ()

keep :: (a -> Bool) -> [a] -> [a]
keep p = foldr concatIf []
  where
    concatIf a as = if p a then a : as else as