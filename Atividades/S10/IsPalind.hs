module Atividades.S09.Palind where

isPalind xs = reverse xs == xs

main = do
  a <- getLine
  print $ isPalind a
