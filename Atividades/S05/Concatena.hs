module Atividades.S05.Concatena where

concatena [] xs = xs
concatena (x : xs1) xs2 = x : concatena xs1 xs2

main = do
  a <- readLn :: IO [Int]
  b <- readLn :: IO [Int]
  print $ concatena a b
