indexOf xs n
  | null xs = -1
  | n == head xs = 0
  | otherwise = if next == -1 then -1 else 1 + next
    where next = indexOf (tail xs) n
      
main :: IO ()
main = do
  xs <- readLn :: IO [Int]
  y <- readLn :: IO Int
  print $ indexOf xs y
