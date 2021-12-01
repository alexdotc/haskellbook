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

--4 TODO come back before ch15
--
-- TODO A bit more complicated but worth attempting as an exercise is
-- changing the game so that, as with normal hangman, only incorrect
-- guesses count towards the guess limit.
