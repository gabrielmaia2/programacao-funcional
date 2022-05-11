module Atividades.S04.Pertence where

pertence _ [] = False
pertence n (x : xs)
  | n == x = True
  | otherwise = pertence n xs

pertence2 n xs = elem n xs

pertence3 n xs = not $ null $ filter (== n) xs

pertence4 n xs = not $ null [x | x <- xs, x == n]

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO [Int]
  print $ pertence a b
