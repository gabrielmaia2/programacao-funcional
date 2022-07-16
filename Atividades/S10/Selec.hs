module Atividades.S09.Selec where

selec _ [] = ""
selec xs (x2 : xs2) = (xs !! x2) : selec xs xs2

main = do
  a <- getLine
  b <- readLn :: IO [Int]
  print $ selec a b
