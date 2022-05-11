module Atividades.S05.Fibonacci where

fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main = do
  a <- readLn :: IO Int
  print $ fib a
