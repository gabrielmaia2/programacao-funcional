module Atividades.S08.ElemIndex where

import Data.Maybe (fromJust, isJust)

myelemIndex _ [] = Nothing
myelemIndex n (x : xs)
  | x == n = Just 0
  | isJust next = Just $ 1 + fromJust next
  | otherwise = Nothing
  where
    next = myelemIndex n xs

main = do
  a <- readLn :: IO Int
  b <- readLn :: IO [Int]
  print $ myelemIndex a b
