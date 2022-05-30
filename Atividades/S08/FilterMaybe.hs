module Atividades.S08.FilterMaybe where

import Data.Maybe (fromJust, isJust)

filterMaybe [] = []
filterMaybe (x : xs) = if isJust x then fromJust x : next else next
  where next = filterMaybe xs
