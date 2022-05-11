module Atividades.S05.Line where

line2 0 = []
line2 1 = [1]
line2 n = [(f + 1) .. (f + n)]
  where
    f = last $ line2 (n - 1)

soma 0 = 0
soma n = n + soma (n - 1)

line 0 = []
line n = [(1 + soma (n - 1)) .. (n + soma (n - 1))]

main = do
  a <- readLn :: IO Int
  print $ line a
