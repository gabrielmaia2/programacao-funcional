module Atividades.S08.SomaMaybe where

import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)

somaMaybe a b
  | isNothing a && isNothing b = Nothing
  | otherwise = Just $ fromMaybe 0 a + fromMaybe 0 b

main = do
  a <- readLn :: IO (Maybe Int)
  b <- readLn :: IO (Maybe Int)
  print $ somaMaybe a b
