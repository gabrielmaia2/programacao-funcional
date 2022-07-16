module Atividades.S12.Expoentes where

expoentes1 a b = if a `mod` b == 0 then 1 + expoentes1 (a `div` b) b else 0

isDivisible a b = if a `mod` b == 0 then 1 else 0

divIter a b = iterate (\(x, y) -> (isDivisible y b + x, y `div` b)) (0, a)

expoentes a b = fst $ last $ takeWhile (\x -> snd x /= 0) $ divIter a b

main = do
  print $ expoentes 7 2
  print $ expoentes 4 2
  print $ expoentes 8 2
  print $ expoentes 24 2
  print $ expoentes 1024 2
  print $ expoentes 150 5
