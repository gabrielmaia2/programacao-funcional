module Atividades.S05.RotEsq where

rotEsq _ [] = []
rotEsq 0 xs = xs
rotEsq i xs = (tail next) ++ [head next]
  where
    next = rotEsq (i - 1) xs

main = do
  a <- readLn :: IO Int
  b <- getLine
  print $ rotEsq a b
