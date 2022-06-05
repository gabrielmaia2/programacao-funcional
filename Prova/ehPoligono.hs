percorrePoligono n (x:xs)
  | null xs = if n == 0 then (-x) else x
  | n == 0 = percorrePoligono (n - 1) xs - x
  | otherwise = percorrePoligono (n - 1) xs + x

checaPossibilidades n xs
  | n >= length xs = True
  | otherwise = (percorrePoligono n xs > 0) && checaPossibilidades (n + 1) xs

ehPoligono xs = checaPossibilidades 0 xs

main :: IO ()
main = do
  xs <- readLn :: IO [Int]
  print $ ehPoligono xs
