module Atividades.S13.QSort where

import qualified Data.Bifunctor

sort p [] = ([], [])
sort p xs
  | head xs <= p = Data.Bifunctor.first (head xs :) (sort p (tail xs))
  | last xs > p = Data.Bifunctor.second (last xs :) (sort p (init xs))
  | otherwise = Data.Bifunctor.bimap (last xs :) (head xs :) (sort p (init $ tail xs))

qsort [] = []
qsort (x : xs) = qsort (fst (sort x xs)) ++ [x] ++ qsort (snd (sort x xs))

main = do
  a <- readLn :: IO [Int]
  print $ qsort a
