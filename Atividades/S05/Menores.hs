module Atividades.S05.Menores where

maior [] = error "empty list"
maior (x : []) = (x, [])
maior (x : xs)
  | x > x2 = (x, x2 : xs2)
  | otherwise = (x2, x : xs2)
  where
    next = maior xs
    x2 = fst next
    xs2 = snd next

-- maneira 1
menores' i [] men = men
menores' i (x : xs) men
  | i == 0 = []
  | length men < i = menores' i xs (x : men)
  | otherwise = menores' i xs $ snd $ maior (x : men)

menores2 n xs = menores' n xs []

-- maneira 2
menores n xs
  | n >= length xs = xs
  | otherwise = menores n $ snd $ maior xs

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO [Int]
  print $ menores a b
