import Control.Monad (forever)
import Data.Char
import System.Exit (exitSuccess)

-- Intermission Check Your Understanding pg 506

--1  forever, when
--2  Data.Bits
--3  types that blacktip defines
--4a Control.Concurrent.MVar, FilePath.System.CurrentOS, Control.Concurrent
--4b Filesystem.writeFile
--4c Control.Monad


-- Chapter Exercises

--1 in cipher directory...
--2,3

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let pali = palistrip line1
  case (pali == reverse pali) of
    True -> putStrLn "It's a palindrome!"
    False -> do putStrLn "Nope!"; exitSuccess

palistrip :: String -> String
palistrip = filter isLower . map toLower

--4

type Name = String
type Age  = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow
                     | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                 "Name was: " ++ show name ++
                " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Name: "
  name <- getLine
  putStr "Age: "
  age <- getLine
  let pers = mkPerson name (read age :: Integer)
  case pers of
    Right p -> putStrLn $ "Yay! Successfully got a person: " ++ (show p)
    Left AgeTooLow   -> putStrLn $ "Not a person: age too low"
    Left NameEmpty   -> putStrLn $ "Not a person: no name"
    Left (PersonInvalidUnknown s) -> putStrLn $ "Not a person: " ++ s

--
-- TODO A bit more complicated but worth attempting as an exercise is
-- changing the game so that, as with normal hangman, only incorrect
-- guesses count towards the guess limit.
