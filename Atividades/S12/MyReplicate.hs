module Atividades.S12.MyReplicate where

myreplicate 0 _ = []
myreplicate a x = x : myreplicate (a - 1) x
