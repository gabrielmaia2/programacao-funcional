module Atividades.S04.Max3 where

mymax [x] = x
mymax (x : xs) = if x > next then x else next
  where
    next = mymax xs

max3 a b c = mymax [a, b, c]

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  c <- readLn :: IO Int
  print $ max3 a b c
