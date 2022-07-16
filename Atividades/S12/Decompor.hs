module Atividades.S12.Decompor where

import Data.List (unfoldr)
import Data.Maybe (fromJust, isNothing)

separa' 0 = []
separa' x = separa' (x `div` 10) ++ [x `mod` 10]

separa 0 = [0]
separa x = separa' x

takeWhile2 f xs = fst (span f xs) ++ [head $ snd $ span f xs]

separa2 x = fst $ last $ takeWhile2 (\x -> snd x /= 0) $ iterate (\x -> ((snd x `mod` 10) : fst x, snd x `div` 10)) ([], x)

separa3 0 = [0]
separa3 x = reverse $ unfoldr (\a -> if a == 0 then Nothing else Just (a `mod` 10, a `div` 10)) x
