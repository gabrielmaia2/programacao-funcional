module Atividades.S03.ContarIguais where

contarIguais a b c
  | a == b && b == c = 3
  | a == b || b == c || a == c = 2
  | otherwise = 0

-- main = do
--   print $ contarIguais 2 2 2 == 3
--   print $ contarIguais 2 2 3 == 2
--   print $ contarIguais 2 3 2 == 2
--   print $ contarIguais 2 1 1 == 2
--   print $ contarIguais 3 2 1 == 0

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  c <- readLn :: IO Int
  print $ contarIguais a b c