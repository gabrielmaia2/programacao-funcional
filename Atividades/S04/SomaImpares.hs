module Atividades.S04.SomaImpares where

somaImpares xs = sum $ filter odd xs

somaImpares2 xs = sum [x | x <- xs, odd x]

main = do
  a <- readLn :: IO [Int]
  print $ somaImpares a
