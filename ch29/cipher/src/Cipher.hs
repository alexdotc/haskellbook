module Main where

import System.Exit (exitSuccess, exitFailure)
import System.IO
import System.Environment
import Control.Monad (forever, replicateM)
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

runVigenere :: Keyword -> String -> ShiftDir -> String
runVigenere  _ [] _ = []
runVigenere  [] s _ = s
runVigenere (c:k) (c':s) d
  | not $ isUppercaseAlpha c' || isLowercaseAlpha c'
              = c'       : (runVigenere (c:k) s d)
  | otherwise = f c c'   : (runVigenere (k ++ [c]) s d)
  where f = (\c -> \c' -> shift c' (shiftN c) d)

vigenere :: Keyword -> String -> String
vigenere k s = runVigenere k s Right'

unvigenere :: Keyword -> String -> String
unvigenere k s = runVigenere k s Left'

doVigenere :: (Keyword -> String -> String) -> Keyword -> String -> IO ()
doVigenere f k i = hPutStrLn stdout $ f k i

main :: IO ()
main = forever $ do
  args <- getArgs
  case length args of
    2 -> return ()
    _ -> do { putStrLn "Must have a mode , then key, try again"; exitFailure }
  case (head args, last args) of
    ("-d", k) -> do { i <- hGetContents stdin; doVigenere unvigenere k i; exitSuccess }
    ("-e", k) -> do { i <- hGetContents stdin; doVigenere vigenere k i; exitSuccess }
    _         -> do { putStrLn "Invalid mode"; exitFailure }

vigenereGen :: Gen (Keyword, String)
vigenereGen = do
  k <- arbitrary
  s <- arbitrary
  return $ (k,s)

prop_Vigenere :: Property
prop_Vigenere = forAll vigenereGen (\(n,s) -> (unvigenere n . vigenere n $ s) == s)

runQC :: IO ()
runQC = do quickCheck prop_Vigenere
