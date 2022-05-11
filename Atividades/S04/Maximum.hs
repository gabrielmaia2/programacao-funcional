module Atividades.S04.Maximum where

maior [] = error "empty list"
maior [x] = x
maior (x : xs) = if max > x then max else x
  where
    max = maior xs

main = do
  a <- readLn :: IO [Int]
  print $ maior a
