module Atividades.S05.Sequencia where

sequencia 0 _ = []
sequencia i n = n : sequencia (i - 1) (n + 1)

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  print $ sequencia a b
