module Atividades.S03.Min3 where

min3 :: Ord a => a -> a -> a -> a
min3 x y z
  | x < y && y < z = x
  | x > y = y
  | x < z = x
  | otherwise = z

-- main = do
--   print $ min3 1 2 3 == 1
--   print $ min3 2 1 3 == 1
--   print $ min3 3 4 2 == 2
--   print $ min3 2 5 4 == 2

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  c <- readLn :: IO Int
  print $ min3 a b c
