module Atividades.S07.EhPrimo where

ehPrimo :: (Integral a) => a -> Bool
ehPrimo 1 = True
ehPrimo 2 = True
ehPrimo n = foldr (\x acc -> acc && (n `mod` x /= 0)) True [2.. ceiling $ sqrt $ fromIntegral n]

main = do
  a <- readLn :: IO Int
  print $ ehPrimo a
