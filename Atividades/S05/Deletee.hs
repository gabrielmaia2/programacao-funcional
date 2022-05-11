module Atividades.S05.Deletee where

deletee _ [] = []
deletee n (x : xs)
  | x == n = xs
  | otherwise = x : deletee n xs

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO [Int]
  print $ deletee a b
