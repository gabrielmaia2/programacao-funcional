module Atividades.S09.Upper where

mapChar c [] = c
mapChar c (x : xs) = if c == fst x then snd x else mapChar c xs

upperChar c = mapChar c $ zip ['a' .. 'z'] ['A' .. 'Z']

upper "" = ""
upper (x : xs) = upperChar x : upper xs

main = do
  a <- getLine
  print $ upper a
