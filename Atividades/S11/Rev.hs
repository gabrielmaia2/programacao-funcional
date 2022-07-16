module Atividades.S11.Rev where

rev' 0 x = x
rev' x a = rev' (x `div` 10) (a * 10 + x `mod` 10)

rev x = rev' x 0

main = do
  a <- readLn :: IO Int
  print $ rev a
