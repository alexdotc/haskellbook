{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Data.Char (toUpper)
import Data.List (sort)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return $ Identity a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

data Sum a b = First a | Second b deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a,
         return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a), (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

data Bool' = True' | False' deriving (Generic)

instance CoArbitrary Bool'

trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary

main :: IO ()
main = do
  sample trivialGen

-- Chapter Exercises
--
-- Validating Numbers into Words (sep file, WordNumberTest.hs)

--Using QuickCheck

runQC :: IO ()
runQC = do
  quickCheck prop_halfIdentity --1
  quickCheck prop_listOrdered --2 
  quickCheck prop_plusAssociative -- 3 
  quickCheck prop_plusCommutative -- 3
  quickCheck prop_multAssociative --4 
  quickCheck prop_multCommutative -- 4
  quickCheck prop_quotrem -- 5, success if not div by zero
  quickCheck prop_divmod -- 5, success if not div by zero
  quickCheck prop_powAssoc -- 6, should fail
  quickCheck prop_powComm -- 6, should fail
  quickCheck prop_reverse -- 7
  quickCheck prop_dollar_conc -- 8
  quickCheck prop_foldr1 -- 9
  quickCheck prop_foldr2 -- 9
  quickCheck prop_n_conc -- 10, very conditional
  quickCheck prop_rs_conc -- 11
  quickCheck prop_f -- Idempotence pg 577 #1
  quickCheck prop_f'_conc -- Idempotence pg 577 #2

--1

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = x == halfIdentity x

--2

listOrdered :: Ord a => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_listOrdered :: [Int] -> Bool
prop_listOrdered xs = listOrdered $ sort xs

--3

plusAssociative :: Integral a => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Integral a => a -> a -> Bool
plusCommutative x y = x + y == y + x

prop_plusAssociative :: Int -> Int -> Int -> Bool
prop_plusAssociative = plusAssociative

prop_plusCommutative :: Int -> Int -> Bool
prop_plusCommutative = plusCommutative

--4

multAssociative :: Integral a => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: Integral a => a -> a -> Bool
multCommutative x y = x * y == y * x

prop_multAssociative :: Int -> Int -> Int -> Bool
prop_multAssociative = multAssociative

prop_multCommutative :: Int -> Int -> Bool
prop_multCommutative = multCommutative

--5

quotrem :: Positive Int -> Positive Int -> Bool
quotrem (Positive x) (Positive y) = (quot x y)*y + (rem x y) == x

divmod :: Positive Int -> Positive Int -> Bool
divmod (Positive x) (Positive y) = (div x y)*y + (mod x y) == x

prop_quotrem = quotrem
prop_divmod = divmod

--6
--Not commutative or associative, should fail

prop_powAssoc :: Int -> Int -> Int -> Bool
prop_powAssoc x y z = (x^y)^z == x^(y^z)

prop_powComm :: Int -> Int -> Bool
prop_powComm x y = x^y == y^x

--7 

rr = reverse . reverse

prop_reverse :: String -> Bool
prop_reverse xs = rr xs == id xs

--8

prop_dollar :: (Eq b) => (a -> b) -> a -> Bool
prop_dollar f a = (f $ a) == f a

prop_dollar' :: (Eq b) => Fun a b -> a -> Bool
prop_dollar' (Fun _ f) a = (f $ a) == f a

prop_dollar_conc = prop_dollar' :: Fun Int String -> Int -> Bool

--9

prop_foldr1 :: [Int] -> [Int] -> Bool
prop_foldr1 one two = foldr (:) one two == (++) two one

prop_foldr2 :: [[Int]] -> Bool
prop_foldr2 one = foldr (++) [] one == concat one

--10

prop_n :: Positive Int -> [a] -> Property
prop_n (Positive n) xs = n <= length xs ==> length (take n xs) == n

prop_n_conc = prop_n :: Positive Int -> [Int] -> Property

--11

prop_rs :: (Read a, Show a, Eq a) => a -> Bool
prop_rs x = (read (show x)) == x

prop_rs_conc = prop_rs :: Int -> Bool

-- Chapter Exercises Idempotence pg 577

twice f = f . f
fourTimes = twice . twice

--1

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (w:ws) = toUpper w : ws

prop_f :: String -> Bool
prop_f x = (capitalizeWord x == twice capitalizeWord x) 
            && (capitalizeWord x == fourTimes capitalizeWord x)

--2

prop_f' :: Ord a => [a] -> Bool
prop_f' x = (sort x == twice sort x) && (sort x == fourTimes sort x)

prop_f'_conc = prop_f' :: [Int] -> Bool

-- Chapter Exercises Make a Gen random generator for the datatype pg 578

data Fool = Fulse | Frue deriving (Eq, Show)

--1

foolgen :: Gen Fool
foolgen = frequency [(1, return Fulse), (1, return Frue)]

--2

foolgen' :: Gen Fool
foolgen' = frequency [(2, return Fulse), (1, return Frue)]

-- Chapter Exercises Hangman Testing pg578-579

-- Added to ch13 hangman module

-- Chapter Exercises Validating Ciphers pg 580

-- Added to ch13 cipher module
