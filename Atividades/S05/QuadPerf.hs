module Atividades.S05.QuadPerf where

quadperf' n i
  | i * i < n = quadperf' n (i + 1)
  | i * i == n = True
  | otherwise = False

quadperf n = quadperf' n 0

main = do
  a <- readLn :: IO Int
  print $ quadperf a
