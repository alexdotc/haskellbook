module Main where

import System.Exit (exitSuccess)
import System.IO
import Control.Monad (forever)
import Data.Char
import Test.QuickCheck

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
doVigenere  [] s _ = s
doVigenere (c:k) (c':s) d
  | not $ isUppercaseAlpha c' || isLowercaseAlpha c'
              = c'       : (doVigenere (c:k) s d)
  | otherwise = f c c'   : (doVigenere (k ++ [c]) s d)
  where f = (\c -> \c' -> shift c' (shiftN c) d)

vigenere :: Keyword -> String -> String
vigenere k s = doVigenere k s Right'

unvigenere :: Keyword -> String -> String
unvigenere k s = doVigenere k s Left'

runVigenere :: IO ()
runVigenere = do
  putStr "Enter keyword for Vigenere cipher: "
  kw <- getLine
  putStr "Enter secret message: "
  msg <- getLine
  putStrLn $ vigenere kw msg

runCaesar :: IO ()
runCaesar = do
  putStr "Enter number of shifts for Caesar cipher: "
  n <- getLine
  let shifts = (read n :: Int) -- it throws an exception if the user doesn't enter an int but oh well, haven't got to exceptions yet
  putStr "Enter secret message: "
  msg <- getLine
  putStrLn $ caesar shifts msg

main :: IO ()
main = forever $ do
  hSetBuffering stdout NoBuffering
  putStr "Which cipher? Caesar or Vigenere: "
  cipher <- getLine
  case fmap toLower cipher of
    "caesar" -> do runCaesar
                   exitSuccess
    "vigenere" -> do runVigenere
                     exitSuccess
    _ -> putStrLn "Choose either Caesar or Vigenere"

-- ch14 exercises

caesarGen :: Gen (Int, String)
caesarGen = do
  n <- arbitrary
  s <- arbitrary
  return $ (n, s)

vigenereGen :: Gen (Keyword, String)
vigenereGen = do
  k <- arbitrary
  s <- arbitrary
  return $ (k,s)

prop_Caesar :: Property
prop_Caesar = forAll caesarGen (\(n,s) -> (uncaesar n . caesar n $ s) == s)

prop_Vigenere :: Property
prop_Vigenere = forAll vigenereGen (\(n,s) -> (unvigenere n . vigenere n $ s) == s)

runQC :: IO ()
runQC = do
  quickCheck prop_Caesar
  quickCheck prop_Vigenere
