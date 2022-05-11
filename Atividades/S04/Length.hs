module Atividades.S04.Length where

total [] = 0
total (x : xs) = 1 + total xs

total2 xs = sum $ map (const 1) xs

total3 xs = foldr (\_ y -> y + 1) 0 xs

main = do
  a <- readLn :: IO [Int]
  print $ total a
