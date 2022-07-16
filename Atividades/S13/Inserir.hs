module Atividades.S13.Inserir where

inserir a [] = [a]
inserir a (x : xs) = if a <= x then a : x : xs else x : inserir a xs

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO [Int]
  print $ inserir a b
