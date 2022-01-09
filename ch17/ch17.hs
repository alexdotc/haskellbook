import Data.List (elemIndex)

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- Exercises Lookups pg 702

--1 

added :: Maybe Integer
added = pure (+3) <*> (lookup 3 $ zip [1,2,3] [4,5,6])

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
maxed = max' <$> x <*> y'

--4

xs = [1,2,3]
ys = [4,5,6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x' <*> y''

-- Exercise Identity Instance pg 704

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

-- Exercises Constant Instance pg 706

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure a = Constant $ mempty
  (Constant a) <*> (Constant b) = Constant $ a <> b

-- Exercises Fixer Upper pg 721

--1

one :: Maybe String
one = const <$> Just "Hello" <*> pure "World"

--2

two :: Maybe (Int, Int, String, [Int])
two = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools), (1, return Twoo) ]

instance Semigroup Bull where
  _ <>  _ = Fools

instance Monoid Bull where
  mempty = Fools

instance EqProp Bull where (=-=) = eq

type SSI = (String, String, Int)

trigger :: [SSI]
trigger = undefined

instance Semigroup a => Semigroup (ZipList a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty

-- Exercise List Applicative pg 734

data List a = Nil | Cons a (List a) deriving (Eq, Show)

listGen :: Arbitrary a => Gen (List a)
listGen = do
  x <- arbitrary
  l <- listGen
  frequency [(1, return $ Nil), (10, return $ Cons x l)]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listGen

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure x = Cons x Nil
  (Cons f fs) <*> (Cons x xs) = Cons (f x) (fmap f xs) `append` (fs <*> (Cons x xs))
  _ <*> _ = Nil

-- Exercise ZipList Applicative pg 736

take' :: Int -> List a -> List a
take' n Nil = Nil
take' n (Cons x xs)
  | n > 0     = Cons x (take' (n-1) xs)
  | otherwise = Nil 

zappend :: ZipList' a -> ZipList' a -> ZipList' a
zappend (ZipList' Nil) ys = ys
zappend (ZipList' xs) (ZipList' ys) = ZipList' $ append xs ys

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ pure x
  (ZipList' (Cons f fs)) 
    <*> (ZipList' (Cons x xs)) = pure (f x) `zappend` 
                                  (ZipList' fs <*> ZipList' xs)
  _ <*> _ = ZipList' Nil

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    l <- arbitrary
    return $ ZipList' l

--Exercises Variations on Either pg 740

data Validation e a = Failure' e | Success' a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Failure' b) = Failure' b
  fmap f (Success' a) = Success' $ f a

instance Monoid e => Applicative (Validation e) where
  pure x = Success' x
  (Success' f) <*> (Success' a) = Success' $ f a
  (Failure' a) <*> (Failure' b) = Failure' (a <> b)
  (Failure' a) <*> (Success' b) = Failure' a
  (Success' b) <*> (Failure' a) = Failure' a

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return $ Success' x), (1, return $ Failure' y)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

type ValidationType = Validation String (Char, Sum Int, String)

validation :: ValidationType
validation = undefined


-- Chapter Exercises Specialize the Types pg 741

--1 

listpure :: a -> [a]
listpure = pure

listap :: [(a -> b)] -> [a] -> [b]
listap = (<*>)

--2

iopure :: a -> IO a
iopure = pure

ioap :: IO (a -> b) -> IO a -> IO b
ioap = (<*>)

--3

tuplepure :: Monoid b => a -> (b, a)
tuplepure = pure

tupleap :: Monoid a => (a, (b -> c)) -> (a, b) -> (a, c)
tupleap = (<*>) 

--4

funpure :: a -> (e -> a)
funpure = pure

funap :: (e -> a -> b) -> (e -> a) -> (e -> b)
funap = (<*>)

-- Chapter Exercises Applicative Instances pg 742

--1

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair a b) = Pair (f a) (g b)

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

type PT = Pair (String, Int, Char)

ptAp :: PT
ptAp = undefined

--2

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x $ f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (Two a f) <*> (Two b c) = Two (a <> b) (f c)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

type TwoT = Two (Sum Int) (String, Int, Char)

twoAp :: TwoT
twoAp = undefined

--3

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y $ f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c)
  where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return $ Three x y z

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (Three a b f) <*> (Three c d e) = Three (a <> c) (b <> d) (f e)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

type ThreeT = Three String (Product Int) (Char, Sum Int, [Int])

threeAp :: ThreeT
threeAp = undefined

--4

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b)
  where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return $ Three' x y z

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' a f g) <*> (Three' b c d) = Three' (a <> b) (f c) (g d)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

type Three'T = Three' String (Char, [String], Int)

three'Ap :: Three'T
three'Ap = undefined

--5

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y $ f z

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) 
  => Arbitrary (Four a b c d) where
    arbitrary = do
      w <- arbitrary
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return $ Four w x y z

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (Four a b c f) <*> (Four d e g h) = Four (a <> d) (b <> e) (c <> g) (f h)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

type FourT = Four String String String (Char, Int, [Int])

fourAp :: FourT
fourAp = undefined

--6

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y $ f z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four' w x y z

instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (Four' a b c f) <*> (Four' d e g h) = Four' (a <> d) (b <> e) (c <> g) (f h)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

type Four'T = Four' String (Char, Int, [Int])

four'Ap :: Four'T
four'Ap = undefined

-- Chapter Exercises Combinations pg 742

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos xs ys zs = liftA3 (,,) xs ys zs

main :: IO ()
main = do
  quickBatch $ monoid Twoo
  quickBatch $ applicative trigger
  --quickBatch $ monoid $ ZipList [1 :: Sum Int]
  --quickBatch $ applicative $ ZipList' $ Cons ("a", "b", "c") Nil -- TODO
  quickBatch $ applicative validation
  quickBatch $ applicative ptAp
  quickBatch $ applicative twoAp
  quickBatch $ applicative threeAp
  quickBatch $ applicative three'Ap
  quickBatch $ applicative fourAp
  quickBatch $ applicative four'Ap
