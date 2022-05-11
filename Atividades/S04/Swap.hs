module Atividades.S04.Swap where

swap xs a b
  | a == b = xs
  | b < a = swap xs b a
  | length xs <= b = xs
  | otherwise = l1 ++ [c2] ++ l2 ++ [c1] ++ l3
  where
    (l1, l21) = splitAt a xs
    l22 = tail l21
    c1 = head l21
    (l2, l31) = splitAt (b - a - 1) l22
    l3 = tail l31
    c2 = head l31

main = do
  a <- readLn :: IO [Int]
  b <- readLn :: IO Int
  c <- readLn :: IO Int
  print $ swap a b c
