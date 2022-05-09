{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative (liftA2, liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Chapter Exercises

-- Traversable Instances

-- Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show, Arbitrary, EqProp)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap :: Monoid m => (a -> m) -> Identity a -> m
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse :: Applicative f => (a -> f b) -> Identity a -> f (Identity b)
  traverse f (Identity a) = Identity <$> f a

-- Constant

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show, Arbitrary, EqProp)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap :: Monoid m => (b -> m) -> Constant a b -> m
  foldMap f (Constant a) = mempty

instance Traversable (Constant a) where
  traverse :: Applicative f => (b -> f c) -> Constant a b -> f (Constant a c)
  traverse f (Constant a) = pure $ Constant a

-- Maybe

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap f (Yep a) = Yep $ f a 
  fmap _ _ = Nada

instance Foldable Optional where
  foldMap f (Yep a) = f a
  foldMap _ _ = mempty

instance Traversable Optional where
  traverse f (Yep a) = Yep <$> f a
  traverse _ _ = pure Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency $ [(5, return a), (1, return Nada)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

-- List

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x y) = Cons (f x) (fmap f y)

lconcat :: Monoid a => List a -> a
lconcat Nil = mempty
lconcat (Cons x y) = x <> lconcat y

instance Foldable List where
  foldMap f xs = lconcat $ f <$> xs

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse f Nil = pure Nil
  traverse f (Cons x y) = liftA2 Cons (f x) (traverse f y)

listGen :: Arbitrary a => Gen (List a)
listGen = do
  x <- arbitrary
  l <- listGen
  frequency [(1, return $ Nil), (10, return $ Cons x l)]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listGen

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- Three

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- Pair

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = (Pair a) <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

-- Big

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b c) = Big a (f b) (f c)

instance Foldable (Big a) where
  foldMap f (Big a b c) = f b <> f c

instance Traversable (Big a) where
  traverse f (Big a b c) = liftA2 (Big a) (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    y' <- arbitrary
    return $ Big x y y'

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

-- Bigger

data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance Foldable (Bigger a) where
  foldMap f (Bigger a b c d) = f b <> f c <> f d

instance Traversable (Bigger a) where
  traverse f (Bigger a b c d) = liftA3 (Bigger a) (f b) (f c) (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    y' <- arbitrary
    y'' <- arbitrary
    return $ Bigger x y y' y''

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

-- S TODO

-- Tree TODO

test :: IO ()
test = do
  let tIdentity :: Identity (Int, Int, [Int])
      tIdentity = undefined
  quickBatch (traversable tIdentity)
  
  let tConstant :: Constant (Int, Int, [Int]) (Int, Int, [Int])
      tConstant = undefined
  quickBatch (traversable tConstant)

  let tOptional :: Optional (Int, Int, [Int])
      tOptional = undefined
  quickBatch (traversable tOptional)

  let tList :: List (Int, Int, [Int])
      tList = undefined
  quickBatch (traversable tList)
  
  let tThree :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])
      tThree = undefined
  quickBatch (traversable tThree)

  let tPair :: Pair (Int, Int, [Int]) (Int, Int, [Int])
      tPair = undefined
  quickBatch (traversable tPair)
  
  let tBig :: Big (Int, Int, [Int]) (Int, Int, [Int])
      tBig = undefined
  quickBatch (traversable tBig)
  
  let tBigger :: Bigger (Int, Int, [Int]) (Int, Int, [Int])
      tBigger = undefined
  quickBatch (traversable tBigger)
