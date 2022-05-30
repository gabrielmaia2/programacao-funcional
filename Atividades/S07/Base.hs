module Atividades.S07.Base where
import Data.Char (intToDigit)
import Data.Char (ord)

-- eu fiz sem fold ou zip porque eu nao vejo como fazer com qualquer um dos dois

toCharDigit n = toEnum (n - 10 + ord 'A') :: Char

toDigit n = if n < 10 then intToDigit n else toCharDigit n

base' 0 _ = ""
base' n b = base' (n `div` b) b ++ [toDigit $ n `mod` b]

base 0 _ = "0"
base n b = base' n b

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  print $ base a b
