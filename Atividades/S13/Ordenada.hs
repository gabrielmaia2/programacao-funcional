module Atividades.S13.Ordenada where

ordenada [] = True
ordenada [_] = True
ordenada (x : xs) = ordenada xs && x <= head xs

main = do
  a <- readLn :: IO [Int]
  print $ ordenada a
