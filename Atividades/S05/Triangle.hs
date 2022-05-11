module Atividades.S05.Triangle where

import Atividades.S05.Line

triangle 0 = []
triangle n = triangle (n - 1) ++ [line n]

main = do
  a <- readLn :: IO Int
  print $ triangle a
