module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
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
  return $ (n+m) `mod` 2 == 1

main :: IO ()
main = do
  scores <- execStateT (morra 3) (0,0)
  print scores
