module Atividades.S07.ElemIndex where

import Data.Maybe (fromJust, isJust)

myelemIndex _ [] = Nothing
myelemIndex n (x : xs)
  | x == n = Just 0
  | isJust next = Just $ 1 + fromJust next
  | otherwise = Nothing
  where
    next = myelemIndex n xs
