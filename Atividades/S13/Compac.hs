module Atividades.S13.Compac where

import Data.List (unfoldr)

-- eu achei que era pra contar assim, mas nao entao fiz de novo

getIncrease2 x res = if length (head res) == 1 then [x, 2] else [x, (head res !! 1) + 1]

increaseCount2 x [] = [[x]]
increaseCount2 x res = if head (head res) == x then getIncrease2 x res : tail res else head res : increaseCount2 x (tail res)

compac2 :: (Eq t, Num t) => [t] -> [[t]]
compac2 = foldr increaseCount2 []

getIncrease val = if length val == 1 then [head val, 2] else [head val, (val !! 1) + 1]

increaseCount x [] = [[x]]
increaseCount x res = if head (last res) == x then init res ++ [getIncrease (last res)] else res ++ [[x]]

compac :: (Eq t, Num t) => [t] -> [[t]]
compac xs = reverse $ foldr increaseCount [] xs

main = do
  a <- readLn :: IO [Int]
  print $ compac a
