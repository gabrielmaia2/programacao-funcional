module Atividades.S05.Frequencia (frequencia) where

frequencia i [] = 0
frequencia i (x : xs)
  | x == i = 1 + frequencia i xs
  | otherwise = frequencia i xs

frequencia2 i xs = length $ filter (== i) xs

frequencia3 i xs = length [x | x <- xs, x == i]

frequencia4 i xs = foldr f 0 xs
  where
    f x acc = if x == i then acc + 1 else acc

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO [Int]
  print $ frequencia a b
