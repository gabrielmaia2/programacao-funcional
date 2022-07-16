module Atividades.S13.Merge where

merge [] [] = []
merge [] a = a
merge a [] = a
merge (x1 : xs1) (x2 : xs2) = if x1 <= x2 then x1 : (merge xs1 (x2 : xs2)) else x2 : (merge (x1 : xs1) xs2)
