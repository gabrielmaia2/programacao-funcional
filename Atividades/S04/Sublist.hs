module Atividades.S04.Sublist where

mymod i xs = if i < 0 then i + length xs else i

sublist' 0 0 _ = []
sublist' 0 b (x : xs) = x : sublist' 0 (b - 1) xs
sublist' a b (_ : xs) = sublist' (a - 1) (b - 1) xs

sublist a b xs = sublist' p1 p2 xs
  where
    p1 = mymod a xs
    p2 = mymod b xs

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  c <- readLn :: IO [Int]
  print $ sublist a b c
