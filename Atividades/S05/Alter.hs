module Atividades.S05.Alter where

alter' n i
  | n < 0 = error "invalid input"
  | i > n = []
  | otherwise = i : (- i) : alter' n (i + 1)

alter n = alter' n 1

main = do
  a <- readLn :: IO Int
  print $ alter a
