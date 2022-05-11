module Atividades.S04.Fatorial where

fatorial n
  | n < 0 = error "there is no factorial for negative integers"
  | n < 2 = 1
  | otherwise = n * fatorial (n - 1)

fatorial2 n = product [1 .. n]

main = do
  a <- readLn :: IO Int
  print $ fatorial a
