module Atividades.S05.MyReplicate where

myreplicate 0 _ = []
myreplicate n i = i : myreplicate (n - 1) i
