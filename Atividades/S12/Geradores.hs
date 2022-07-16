module Atividades.S12.Geradores where

import Data.List (unfoldr)

sign x = x `div` abs x

next1 x = if sign x > 0 then (-1) * x else ((-1) * x) + 1

next2 x = sign x * (abs x + 1) * (-1)

next3 x = x * 2

next4 x = x `div` 2

-- Recursivas

geradorGen1 next x = x : geradorGen1 next (next x)

gerador11 = 0 : geradorGen1 next1 1

gerador12 = geradorGen1 next2 1

gerador13 = geradorGen1 next3 1

-- Unfoldr

geradorGen2 f = unfoldr (\a -> Just (a, f a))

gerador21 = 0 : geradorGen2 next1 1

gerador22 = geradorGen2 next2 1

gerador23 = geradorGen2 next3 1

-- Iterate
gerador31 = 0 : iterate next1 1

gerador32 = iterate next2 1

gerador33 = iterate next3 1

-- Divisao por 2

gerador4 x = takeWhile (/= 0) $ iterate next4 x
