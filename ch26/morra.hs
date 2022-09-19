module Main where

import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import System.Exit (exitSuccess)
import System.IO
import System.Random (randomRIO)

type Morra = StateT (PS,CS) IO PlayerWon

type PS = Int
type CS = Int
type PlayerWon = Bool

morra :: Int -> Morra
morra n = do
  v <- liftIO (playMorra n)
  case v of
    True -> state $ \(p,c) -> (v, (p+1,c))
    False -> state $ \(p,c) -> (v, (p,c+1))

playMorra :: Int -> IO PlayerWon
playMorra n = do
  m <- randomRIO (1, 10)
  putStrLn $ "You guessed: " ++ show n
  putStrLn $ "Computer guessed: " ++ show m
  return $ (n+m) `mod` 2 == 1

pprintScores :: (PS,CS) -> IO ()
pprintScores (ps,cs) = putStrLn $ "Your score: " ++ show ps 
    ++ "\tComputer score: " ++ show cs ++ "\n"

finalScores :: (PS,CS) -> IO ()
finalScores (ps,cs)
  | ps > cs = putStrLn "Congratulations! You won!"
  | ps < cs = putStrLn "The computer won! You failed!"
  | otherwise = putStrLn "This game of Morra is tied!"

morraLoop :: (PS,CS) -> IO ()
morraLoop s = do
    putStr "Guess a number between 1 and 10 ('q' to quit): " 
    n <- hGetLine stdin
    case n of
      "q" -> do finalScores s; putStrLn ""; exitSuccess
      _   -> return ()
    (result, scores) <- runStateT (morra $ read n) s
    case result of
      True -> putStrLn "You win this round!"
      False -> putStrLn "You lose this round!"
    pprintScores scores
    morraLoop scores

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Morra! You are odds, computer is evens. FIGHT!"
  morraLoop (0,0)
