module Atividades.S05.RotDir where

rotDir _ [] = []
rotDir 0 xs = xs
rotDir i xs = last next : init next
  where
    next = rotDir (i - 1) xs

main = do
  a <- readLn :: IO Int
  b <- getLine
  print $ rotDir a b
