module Atividades.S05.RemoverMaior where

maior [] = error "empty list"
maior (x : []) = (x, [])
maior (x : xs)
  | x > x2 = (x, x2 : xs2)
  | otherwise = (x2, x : xs2)
  where
    next = maior xs
    x2 = fst next
    xs2 = snd next

removerMaior xs = snd $ maior xs

main = do
  a <- readLn :: IO [Int]
  print $ removerMaior a
