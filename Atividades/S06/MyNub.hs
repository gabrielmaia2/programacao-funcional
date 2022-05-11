module Atividades.S06.MyNub where

remove n [] = []
remove n (x : xs)
  | x == n = next
  | otherwise = x : next
  where
    next = remove n xs

mynub [] = []
mynub (x : xs) = x : mynub (remove x xs)

mynub2 [] = []
mynub2 (x : xs) = x : mynub2 (filter (/= x) xs)

main = do
  a <- readLn :: IO [Int]
  print $ mynub a
