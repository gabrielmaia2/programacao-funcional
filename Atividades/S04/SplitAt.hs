module Atividades.S04.SplitAt where

divide xs 0 = ([], xs)
divide (x : xs) n = (x : fst next, snd next)
  where
    next = divide xs (n - 1)

main = do
  a <- readLn :: IO [Int]
  b <- readLn :: IO Int
  print $ divide a b
