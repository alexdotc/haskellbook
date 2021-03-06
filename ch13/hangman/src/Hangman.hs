module Hangman where

import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import System.Random (randomRIO)

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

newtype WordList = WordList [String] deriving (Eq, Show)

minWordLength :: Int
maxWordLength :: Int

minWordLength = 5
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String) 
          in    l >= minWordLength
             && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar c = fromMaybe '_' c

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = elem c g

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = elem c s

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c:s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c)
            word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
                 \ character, pick \
                 \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "That character was in the\
               \ word, filling in the word \
               \ accordingly"
      return (fillInCharacter puzzle guess)
    (False,_) -> do
      putStrLn "This character wasn't in\
                \ the word, try again."
      return (fillInCharacter puzzle guess)
