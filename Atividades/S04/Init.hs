module Atividades.S04.Init where

primeiros [] = error "empty list"
primeiros [_] = []
primeiros (x : xs) = x : primeiros xs

main = do
  a <- readLn :: IO [Int]
  print $ primeiros a
