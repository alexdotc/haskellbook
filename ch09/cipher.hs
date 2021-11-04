module Cipher where

import Data.Char

data ShiftDir = Left' | Right'

shift :: Char -> Int -> ShiftDir -> Char
shift c n d
  | ascii >= 65 && ascii <= 90  = go $ ord 'A'
  | ascii >= 97 && ascii <= 122 = go $ ord 'a'
  | otherwise = c
  where ascii = ord c
        dir = case d of
                Left' -> (-)
                Right' -> (+)
        go = \asciiBase -> chr $ (dir (ascii - asciiBase) n) `mod` 26 + asciiBase

caesar :: Int -> String -> String
caesar n s = map (\c -> shift c n Right') s

uncaesar :: Int -> String -> String
uncaesar n s = map (\c -> shift c n Left') s
