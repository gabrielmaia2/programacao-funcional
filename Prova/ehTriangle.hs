ehTriangle a b c = (a + b > c) && (b + c > a) && (a + c > b)

main :: IO ()
main = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  c <- readLn :: IO Int
  print $ ehTriangle a b c
