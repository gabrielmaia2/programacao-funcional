module Atividades.S05.Listacc where

listacc' _ [] = []
listacc' n (x : xs) = (x + n) : listacc' (x + n) xs

listacc xs = listacc' 0 xs

main = do
  a <- readLn :: IO [Int]
  print $ listacc a
