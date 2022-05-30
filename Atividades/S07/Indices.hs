module Atividades.S07.Indices where

import Debug.Trace

indices i xs = map snd $ filter (\(a, b) -> a == i) $ zip xs [0,1..]

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO [Int]
  print $ indices a b
