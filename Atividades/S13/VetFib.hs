module Atividades.S13.VetFib where

vetFib 1 = [0]
vetFib 2 = [0, 1]
vetFib 3 = [0, 1, 1]
vetFib n = xs ++ [(last xs) + last (init xs)]
  where
    xs = vetFib (n - 1)

main = do
  a <- readLn :: IO Int
  print $ vetFib a
