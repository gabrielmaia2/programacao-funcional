module Atividades.Challenges.RobotDistance where
import Data.Char (isDigit, digitToInt, intToDigit)
import Debug.Trace (traceShowId)

-- pega valores possiveis que nao estao na lista
pegaVals l s = filter (\x -> not $ elem x s) $ map intToDigit [0..l]

-- pega posicoes possiveis que nao estao na lista
pegaPos xs = map snd $ filter (\(x, _) -> x == '.') $ zip xs [0..length xs]
pegaPoss l xs = pegaPos $ take (l + 1) xs

replaceAt i n xs = a ++ [n] ++ tail b
  where (a, b) = splitAt i xs

robIt _ [] _ = []
robIt l (c:s) i = if isDigit c then (i `mod` (l + 1), c) : next else next
  where next = robIt l s (i + 1)

replaceValues l (i, n) s
  | i >= length s = s
  | otherwise = replaceValues l (i + l + 1, n) $ replaceAt i n s

robotInitial l s = foldr (replaceValues l) s $ robIt l s 0

robot l s = foldr (replaceValues l) ns $ zip (pegaPoss l ns) (pegaVals l ns)
  where ns = robotInitial l s

getHoles xs = pegaPos xs

mainSolver s l = robot l s

getHolesTest :: IO ()
getHolesTest = do
  print $ getHoles "12.3.." == [2,4,5]
  print $ getHoles "12.3.4" == [2,4]
  print $ getHoles "...3.4" == [0,1,2,4]

mainTest :: IO ()
mainTest = do
  print $ mainSolver "01.2." 3 == "01320"
  print $ mainSolver ".0..231..5" 5 == "1045231045"
  print $ mainSolver "2..0..............3..........." 3 == "213021302130213021302130213021"
  print $ mainSolver "0..32..41." 5 == "0413250413"
  print $ mainSolver "9....7.620.5318....." 9 == "95318746209531874620"
