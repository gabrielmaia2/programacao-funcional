combina v l
  | null l = []
  | otherwise = (v : (head l)) : ((combina v) $ tail l)

criaCombs n xs
  | n == 1 = map (\x -> [x]) xs
  | length xs < n = []
  | otherwise = ((combina (head xs)) ((criaCombs (n - 1)) (tail xs))) ++ ((criaCombs n) (tail xs))

ehTriangulo a b c = (a + b > c) && (b + c > a) && (a + c > b)

triangulo x = ehTriangulo (x !! 0) (x !! 1) (x !! 2)

combi xs = map (\x -> (x !! 0, x !! 1, x !! 2)) $ filter (\x -> triangulo x) (criaCombs 3 xs)

main :: IO ()
main = do
  xs <- readLn :: IO [Int]
  print $ combi xs
