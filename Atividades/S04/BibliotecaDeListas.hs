module Atividades.S04.BibliotecaDeListas where

-- ++
conc [] l2 = l2
conc (x : xs) l2 = x : conc xs l2

-- head
myhead [] = error "empty list"
myhead (x : _) = x

-- last
mylast [] = error "empty list"
mylast [x] = x
mylast (_ : xs) = mylast xs

-- tail
mytail [] = error "empty list"
mytail (_ : xs) = xs

-- init
myinit [] = error "empty list"
myinit [_] = []
myinit (x : xs) = x : myinit xs

-- null
mynull [] = True
mynull _ = False

-- length
mylength [] = 0
mylength (x : xs) = 1 + mylength xs
