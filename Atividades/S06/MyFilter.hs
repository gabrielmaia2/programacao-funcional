module Atividades.S06.MyFilter where

myfilter f [] = []
myfilter f (x : xs)
  | f x = x : next
  | otherwise = next
  where
    next = myfilter f xs

main = do
  print $ myfilter (> 5) [0 .. 10] == [6, 7, 8, 9, 10]
  print $ myfilter (odd) [0 .. 10] == [1, 3, 5, 7, 9]
