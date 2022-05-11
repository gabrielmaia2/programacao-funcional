module Atividades.S05.Intercal where

intercal' [] xs _ = xs
intercal' xs [] _ = xs
intercal' (x1 : xs1) (x2 : xs2) i
  | i == 0 = x1 : intercal' xs1 (x2 : xs2) 1
  | otherwise = x2 : intercal' (x1 : xs1) xs2 0

intercal xs1 xs2 = intercal' xs1 xs2 0

intercal2 xs1 xs2
  | length xs1 < length xs2 = intercal2 xs2 xs1
  | otherwise = foldr (\(a, b) acc -> a : b : acc) [] (zip xs1 xs2) ++ drop (length xs2) xs1

main = do
  a <- readLn :: IO [Int]
  b <- readLn :: IO [Int]
  print $ intercal a b
