module Atividades.S07.FractionReduce where

-- reduz fracao por um valor especifico
reducex x (m, n) = if (m `mod` x == 0) && (n `mod` x == 0) then reducex x (m `div` x, n `div` x) else (m, n)

-- reduz fracao onde m e maior que n
reduce' m n = foldr reducex (m, n) [2.. ceiling $ sqrt $ fromIntegral n]

-- reduz fracao
reduce (m, n)
  | n > m =  (snd inv, fst inv)
  | otherwise = reduce' m n
    where inv = reduce' n m

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  print $ reduce (a,b)
