module Atividades.S12.Factors where

isDiv a b = a `mod` b == 0

insNextVal x = (if count == 0 then init x else x) ++ [(fac + 1, 0)]
  where
    fac = fst $ last x
    count = snd $ last x

checkFactor n x = if n `isDiv` fac then (init x ++ [(fac, count + 1)], n `div` fac) else (insNextVal x, n)
  where
    fac = fst $ last x
    count = snd $ last x

factors' x acc = if x < (fst $ last acc) then acc else factors' nextX nextAcc
  where
    nextX = snd $ checkFactor x acc
    nextAcc = fst $ checkFactor x acc

factors x = factors' x [(2, 0)]

main = do
  a <- readLn :: IO Int
  print $ factors a
