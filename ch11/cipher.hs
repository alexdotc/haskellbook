module Cipher where

import Data.Char

data ShiftDir = Left' | Right'
type Keyword = String

isUppercaseAlpha :: Char -> Bool
isUppercaseAlpha c = ascii >= 65 && ascii <= 90
                       where ascii = ord c

isLowercaseAlpha :: Char -> Bool
isLowercaseAlpha c = ascii >= 97 && ascii <= 122
                       where ascii = ord c

shift :: Char -> Int -> ShiftDir -> Char
shift c n d
  | isUppercaseAlpha c = go $ ord 'A'
  | isLowercaseAlpha c = go $ ord 'a'
  | otherwise = c
  where ascii = ord c
        dir = case d of
                Left' -> (-)
                Right' -> (+)
        go = \asciiBase -> chr $ (dir (ascii - asciiBase) n) `mod` 26 + asciiBase

shiftN :: Char -> Int
shiftN c
  | isUppercaseAlpha c = ascii - 65
  | isLowercaseAlpha c = ascii - 97
  | otherwise = 0
  where ascii = ord c

caesar :: Int -> String -> String
caesar n s = map (\c -> shift c n Right') s

uncaesar :: Int -> String -> String
uncaesar n s = map (\c -> shift c n Left') s

doVigenere :: Keyword -> String -> ShiftDir -> String
doVigenere  _ [] _ = []
doVigenere (c:k) (c':s) d
  | not $ isUppercaseAlpha c' || isLowercaseAlpha c'
              = c'       : (doVigenere (c:k) s d)
  | otherwise = f c c'   : (doVigenere (k ++ [c]) s d)
  where f = (\c -> \c' -> shift c' (shiftN c) d)

vigenere :: Keyword -> String -> String
vigenere k s = doVigenere k s Right'

unvigenere :: Keyword -> String -> String
unvigenere k s = doVigenere k s Left'
