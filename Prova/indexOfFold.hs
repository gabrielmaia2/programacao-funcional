indexOf xs n = foldr (f n) (-1) xs

f n el acc
  | n == el = 0
  | otherwise = if acc == -1 then -1 else 1 + acc

main :: IO ()
main = do
  xs <- readLn :: IO [Int]
  y <- readLn :: IO Int
  print $ indexOf xs y
