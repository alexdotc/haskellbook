import Control.Applicative
import Data.List (elemIndex)
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Exercises: Lookups

--1

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

--2

y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

--3

x :: Maybe Int
x = elemIndex 3 [1,2,3,4,5]

y' :: Maybe Int
y' = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max'<$> x <*> y'

--4

xs = [1,2,3]
ys = [4,5,6]

x'' :: Maybe Integer
x''=  lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x'' <*> y''

-- Exercise Identity Instances

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

-- Exercises Constant Instances

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure x = Constant mempty
  Constant f <*> Constant x = Constant (f <> x)

-- Exercise Fixer Upper

--1

fu1 = const <$> Just "Hello" <*> pure "World"

--2
fu2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]

-- Exercise List Applicative

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

instance Applicative List where
  pure x = Cons x Nil
  fs <*> xs = flatMap (\f -> fmap f xs) fs
  -- noob method  
  -- (Cons f fs) <*> xs = append (f <$> xs) (fs <*> xs)

listGen :: Arbitrary a => Gen (List a)
listGen = do
  x <- arbitrary
  l <- listGen
  frequency [(1, return $ Nil), (10, return $ Cons x l)]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listGen

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- Exercise ZipList Applicative

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _   = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

instance Applicative ZipList' where
  pure x = ZipList' $ repeat' x
  ZipList' fs <*> ZipList' xs = ZipList' $ g fs xs
    where g (Cons f fs) (Cons x xs) = Cons (f x) (g fs xs)
          g _ _                     = Nil
  
instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    l <- arbitrary
    return $ ZipList' l

-- Exercise Variations on Either

data Validation e a = Failure' e | Success' a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Success' a) = Success' $ f a
  fmap _ (Failure' e) = Failure' e

instance Monoid e => Applicative (Validation e) where
  pure x = Success' x
  Success' f <*> Success' x = Success' $ f x
  Failure' f <*> Failure' x = Failure' $ f <> x
  _ <*> Failure' x = Failure' x
  Failure' x <*> _ = Failure' x

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    frequency [(1, return $ Success' a), (1, return $ Failure' e)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

-- Chapter Exercises

--A
--1 
listpure = pure :: a -> [a]
listap   = (<*>) :: [(a -> b)] -> [a] -> [b]

--2 
iopure = pure :: a -> IO a
ioap   = (<*>) :: IO (a -> b) -> IO a -> IO b

--3 
tupure :: Monoid b => a -> (b, a)
tupure x = (mempty, x) 

tuap :: Monoid d => (d, a -> b) -> (d, a) -> (d, b)
tuap   = (<*>) 

--4 
funpure = pure :: a -> e -> a
funap   = (<*>) :: (e -> a -> b) -> (e -> a) -> (e -> b)

--B
--1
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure x = Pair x x
  Pair f g <*> Pair x y = Pair (f x) (g y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary =  do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

--2
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  Two z f <*> Two x y = Two (z <> x) (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary =  do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

--3
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  Three w z f <*> Three x y t = Three (w <> x) (z <> y) (f t)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary =  do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

--4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' x f g <*> Three' a b c = Three' (x <> a) (f b) (g c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary =  do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

--5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  Four a b c f <*> Four w x y z = Four (a <> w) (b <> x) (c <> y) (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary =  do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z 

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

--6
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  Four' x y z f <*> Four' a b c d = Four' (x <> a) (y <> b) (z <> c) (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary =  do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four' w x y z

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

-- Combinations

stops = "pbtdkg"
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos x y z = liftA3 (,,) x y z 

-- checkers (QuickCheck)

check :: IO ()
check = do
  print $ combos stops vowels stops
  quickBatch $ applicative $ Pair ((-1 :: Integer), "agasdfa", 's') ((-1 :: Integer), "agasdfa", 's') 
  quickBatch $ applicative $ Two ("hello", "agasdfa", "s") ("agasdfa", 's', Sum (5 :: Integer)) 
  quickBatch $ applicative $ Three (Sum (1 :: Integer), "agasdfa", Just "hello") (Just "no", "agasdfa", Just "no") ((-1 :: Integer), "agasdfa", 's')
  quickBatch $ applicative $ Three' (Sum (1 :: Integer), "agasdfa", [5,6,7,8] :: [Int]) (Sum (3 :: Integer), "agasdfa", [1,2,3,4] :: [Int]) (Sum (-1 :: Integer), "agasdfa", [10,0,-2,3] :: [Int])
  quickBatch $ applicative $ Four (Just "maybe", "agasdfa", Just "hello") (Just "no", "agasdfa", Just "no") (Just "no", "agasdfa", Just "no") ((-1 :: Integer), "agasdfa", Just "bad")
  quickBatch $ applicative $ Four' (Just "maybe", "agasdfa", Just "hello") (Just "no", "agasdfa", Just "no") (Just "no", "agasdfa", Just "no") ((-1 :: Integer), "agasdfa", Just "bad")
  quickBatch $ applicative $ (Success' (Sum (6 :: Integer), "hello", '3') :: Validation String (Sum Integer, [Char] , Char))
  quickBatch $ applicative $ Cons ((5 :: Int),  "bye", 'a') Nil
  quickBatch $ applicative $ ZipList' $ Cons ((5 :: Int),  "bye", 'a') Nil
