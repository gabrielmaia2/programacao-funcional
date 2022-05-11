module Atividades.S05.MaioresQue where

maioresQue _ [] = []
maioresQue n (x : xs)
  | x > n = x : maioresQue n xs
  | otherwise = maioresQue n xs

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO [Int]
  print $ maioresQue a b
