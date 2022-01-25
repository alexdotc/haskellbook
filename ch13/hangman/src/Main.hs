module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (nub)
import Hangman (handleGuess, randomWord', freshPuzzle, Puzzle (..))
import System.Exit (exitSuccess)

gameOver :: Puzzle -> IO ()
gameOver p@(Puzzle wordToGuess _ guessed) =
  if (incorrectGuesses p) >= 10 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

incorrectGuesses :: Puzzle -> Int
incorrectGuesses (Puzzle _ correct all) = length all - length (unique correct)
  where unique xs = nub $ filter (/= Nothing) xs

gameWin :: Puzzle -> IO ()
gameWin (Puzzle wordToGuess filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       putStrLn $ "You found all letters of: " ++ wordToGuess
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
