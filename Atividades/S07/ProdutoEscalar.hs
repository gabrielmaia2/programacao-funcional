module Atividades.S07.ProdutoEscalar where

produtoEscalar' [] [] = 0
produtoEscalar' [] xs = error "Both lists should have same length"
produtoEscalar' xs [] = error "Both lists should have same length"
produtoEscalar' (x1:xs1) (x2:xs2) = x1 * x2 + produtoEscalar' xs1 xs2

produtoEscalar xs1 xs2
  | length xs1 /= length xs2 = error "Both lists should have same length"
  | otherwise = foldr (\(a, b) x -> x + a * b) 0 $ zip xs1 xs2

main = do
  a <- readLn :: IO [Int]
  b <- readLn :: IO [Int]
  print $ produtoEscalar' a b
