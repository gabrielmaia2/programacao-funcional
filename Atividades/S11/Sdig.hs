module Atividades.S11.Sdig where

sdig 0 = 0
sdig x = x `mod` 10 + sdig (x `div` 10)

main = do
  a <- readLn :: IO Int
  print $ sdig a
