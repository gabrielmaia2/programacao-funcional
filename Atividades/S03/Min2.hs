module Atividades.S03.Min2 where

min2 x y
  | x < y = x
  | otherwise = y

-- main = do
--   print $ min2 3 4 == 3
--   print $ min2 4 1 == 1
--   print $ min2 2 2 == 2
--   print $ min2 4 (-1) == (-1)

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  print $ min2 a b
