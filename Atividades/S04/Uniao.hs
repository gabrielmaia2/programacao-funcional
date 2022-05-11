module Atividades.S04.Uniao where

myset [] = []
myset (x : xs) = if elem x xs then myset xs else x : myset xs

uniao l1 l2 = myset $ l1 ++ l2

main = do
  a <- readLn :: IO [Int]
  b <- readLn :: IO [Int]
  print $ uniao a b
