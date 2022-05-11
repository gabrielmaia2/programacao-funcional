module Atividades.S04.Partition where

splitints f xs
  | null xs = ([], [])
  | otherwise = if f x then (x : fst next, snd next) else (fst next, x : snd next)
  where
    next = splitints f $ tail xs
    x = head xs

main = do
  a <- readLn :: IO [Int]
  print $ splitints a
