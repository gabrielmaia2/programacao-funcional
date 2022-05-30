module Atividades.S07.Vigenere where
import Data.Char (ord)

valc c = ord c - ord 'A'
toValc = map valc
charc n = toEnum (n + ord 'A') :: Char

vigenere t c = foldr (\(t, c) acc -> charc ((t + c) `mod` 26) : acc) "" $ zip (toValc t) (toValc $ cycle c)

main = do
  print $ vigenere "ATACARBASESUL" "LIMAO" == "LBMCOCJMSSDCX"
  print $ vigenere "ABACATE" "A" == "ABACATE"
  print $ vigenere "ABACATE" "B" == "BCBDBUF"
  print $ vigenere "ABACATE" "AB" == "ACADAUE"
