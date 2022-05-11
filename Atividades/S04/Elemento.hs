module Atividades.S04.Elemento where

elemento' i xs
  | null xs = error "index out of bounds"
  | i == 0 = head xs
  | otherwise = elemento' (i - 1) (tail xs)

elemento i xs = elemento' pos xs
  where
    pos = if i < 0 then length xs + i else i

elemento2 i xs = xs !! pos
  where
    pos = if i < 0 then length xs + i else i

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO [Int]
  print $ elemento a b
