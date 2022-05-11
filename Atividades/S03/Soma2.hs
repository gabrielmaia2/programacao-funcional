module Atividades.S03.Soma2 where

soma2 x y = x + y

-- main = do
--   print $ soma2 2 6 == 8
--   print $ soma2 (-4) 6 == 2
--   print $ soma2 (-1) (-1) == (-2)

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  print $ soma2 a b
