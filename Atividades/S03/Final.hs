module Atividades.S03.Final where

final :: Int -> [Int] -> [Int]
final n xs
  | n == (length xs) = xs
  | otherwise = final n $ tail xs

-- main = do
--   print $ final 0 [] == []
--   print $ final 3 [2,5,4,7,9,6] == [7,9,6]
--   print $ final 2 [2,5,4,7,9,6] == [9,6]
--   print $ final 1 [2,5,4,7,9,6] == [6]

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO [Int]
  print $ final a b
