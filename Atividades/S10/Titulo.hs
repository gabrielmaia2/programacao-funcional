module Atividades.S09.Titulo where

import Atividades.S09.MetodosString (splitOn)
import Atividades.S09.Upper (mapChar, upperChar)

lowerChar c = mapChar c $ zip ['A' .. 'Z'] ['a' .. 'z']

lower "" = ""
lower (x : xs) = lowerChar x : lower xs

upperFirst "" = ""
upperFirst (x : xs) = upperChar x : xs

upperFirstLowerRest xs = upperFirst $ lower xs

titulo' xs = foldr (\x y -> upperFirstLowerRest x ++ " " ++ y) "" $ splitOn ' ' xs

titulo xs = init $ titulo' xs

main = do
  a <- getLine
  print $ titulo a
