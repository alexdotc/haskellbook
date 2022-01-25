module Main where

import Hangman
import Data.Char (toLower)
import Test.QuickCheck

puzzleGen :: Gen Puzzle
puzzleGen = undefined -- TODO

main :: IO ()
main = do
  putStrLn "working"
