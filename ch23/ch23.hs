{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie: non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 2
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _)  = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          |otherwise =
            let (die, nextGen) = randomR (1,6) gen
            in go (sum+die) (count+1) nextGen

-- Exercises Roll Your Own
--1
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= n = count
          | otherwise =
            let (die, nextGen) = randomR (1,6) gen
            in go (sum+die) (count+1) nextGen

--2
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 g []
  where go :: Int -> Int -> StdGen -> [Die] -> (Int, [Die])
        go sum count gen dies
          | sum >= n = (count, dies)
          | otherwise =
            let (die, nextGen) = randomR (1,6) gen
            in go (sum+die) (count+1) nextGen (intToDie die : dies)


-- Write State for Yourself

newtype State' s a = State' { runState' :: s -> (a, s) }

instance Functor (State' s) where
  fmap :: (a -> b) -> State' s a -> State' s b
  fmap f (State' g) = State' $ \x -> let (a, s) = g x in (f a, s)

instance Applicative (State' s) where
  pure :: a -> State' s a
  pure x = State' $ \s -> (x, s)
  (<*>) :: State' s (a -> b) -> State' s a -> State' s b
  (State' f) <*> (State' g) = State' $ \x -> ((fst $ f x) (fst $ g x), snd (g $ snd $ f x))

instance Monad (State' s) where
  return = pure
  (>>=) :: State' s a -> (a -> State' s b) -> State' s b
  (State' f) >>= g = State' $ \x -> let (a, s) = f x in runState' (g a) s

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

-- Fizzbuzz Differently
fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo z a
  | a < z = []
  | otherwise = fizzBuzz z : fizzbuzzFromTo (z+1) a


main :: IO ()
main = mapM_ putStrLn $ fizzbuzzFromTo 1 100

-- Chapter Exercises

--1
get' :: State' s s
get' = State' $ \s -> (s, s)

--2
put' :: s -> State' s ()
put' s = State' $ \_ -> ((), s)

--3
exec :: State' s a -> s -> s
exec (State' sa) s = let (x, y) = sa s in y

--4
eval :: State' s a -> s -> a
eval (State' sa) = \s -> let (x, y) = sa s in x

--5
modify' :: (s -> s) -> State' s ()
modify' = \f -> State' $ \s -> ((), f s)
