module Atividades.S04.Intersec where

intersec [] _ = []
intersec (x : l1) l2 = if elem x l2 then x : intersec l1 l2 else intersec l1 l2

main = do
  a <- readLn :: IO [Int]
  b <- readLn :: IO [Int]
  print $ intersec a b
