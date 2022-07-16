module Atividades.S11.Base where

import Data.List (unfoldr)

chars = ['0' .. '9'] ++ ['A' .. 'Z']

base 0 _ = "0"
base x b = reverse $ unfoldr (\a -> if a == 0 then Nothing else Just (chars !! (a `mod` b), a `div` b)) x

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  print $ base a b
