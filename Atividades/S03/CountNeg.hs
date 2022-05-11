module Atividades.S03.CountNeg where

countNeg xs = length $ filter (< 0) xs

-- main = do
--   print $ countNeg [] == 0
--   print $ countNeg [1, 2, 3, 4, 5] == 0
--   print $ countNeg [1, -1, 2, -3, 4] == 2
--   print $ countNeg [2, -2] == 1
--   print $ countNeg [1, -1] == 1
--   print $ countNeg [1, -3, -4, 3, 4, -5] == 3

main = do
  a <- readLn :: IO [Int]
  print $ countNeg a
