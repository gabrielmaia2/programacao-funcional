module Atividades.S04.Paridade where

paridade [] = False
paridade (x : xs) = if x then not $ paridade xs else paridade xs

main = do
  a <- readLn :: IO [Bool]
  print $ paridade a
