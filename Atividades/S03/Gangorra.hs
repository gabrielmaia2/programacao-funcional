module Atividades.S03.Gangorra where

gangorra p1 c1 p2 c2
  | res > 0 = 1
  | res < 0 = -1
  | otherwise = 0
  where
    res = p2 * c2 - p1 * c1

-- main = do
--   print $ gangorra 30 100 60 50 == 0
--   print $ gangorra 40 40 38 60 == 1
--   print $ gangorra 35 80 35 75 == -1
--   print $ gangorra 45 23 96 12 == 1
--   print $ gangorra 74 12 65 48 == 1
--   print $ gangorra 78 45 12 23 == -1

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  c <- readLn :: IO Int
  d <- readLn :: IO Int
  print $ gangorra a b c d
