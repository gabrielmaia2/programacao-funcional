module Atividades.S05.Unico where

import Atividades.S05.Frequencia

unico2 i xs = frequencia i xs == 1

unico3 i xs = length (filter (== i) xs) == 1

unico' n [] 1 = True
unico' n [] _ = False
unico' n (x : xs) i
  | i > 1 = False
  | n == x = unico' n xs (i + 1)
  | otherwise = unico' n xs i

unico n xs = unico' n xs 0

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO [Int]
  print $ unico a b
