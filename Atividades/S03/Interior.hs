module Atividades.S03.Interior where

interior' (x : xs)
  | null xs = []
  | otherwise = x : interior' xs

interior (x : xs) = interior' xs

-- main = do
--   print $ null $ interior [1,2]
--   print $ interior [1,3,2] == [3]
--   print $ interior [2,5,3,7,3] == [5,3,7]
--   print $ interior [2,2,2,4] == [2,2]
--   print $ interior [1,2,3,5,7,8] == [2,3,5,7]

main = do
  a <- readLn :: IO [Int]
  print $ interior a
