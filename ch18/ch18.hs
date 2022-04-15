import Control.Applicative
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Bind in terms of fmap and join

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

-- Exercise Either Monad

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second $ f b
  fmap _ (First b) = First b

instance Applicative (Sum a) where
  pure = Second
  Second f <*> Second x = Second $ f x
  First x <*> _         = First x
  Second x <*> First y  = First y

instance Monad (Sum a) where
  return = pure
  Second x >>= f = f x
  First x >>= f  = First x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency $ [(1, return $ First x), (1, return $ Second y)]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

--Chapter Exercises

--1
data Nope a = NopeDotJpg

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

--2
data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' $ f a
  fmap _ (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  Left' f <*> Left' x = Left' $ f x
  Right' x <*> _      = Right' x
  Left' x <*> Right' y = Right' y

instance Monad (PhhhbbtttEither b) where
  return = pure
  Left' x >>= f = f x
  Right' x >>= _ = Right' x 

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return $ Left' x), (1, return $ Right' y)]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

--3
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    i <- arbitrary
    return $ Identity i

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

--4
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

instance Monad List where
  return = pure
  xs >>= f = concat' . fmap f $ xs

listGen :: Arbitrary a => Gen (List a)
listGen = do
  x <- arbitrary
  l <- listGen
  frequency [(1, return $ Nil), (10, return $ Cons x l)]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listGen

instance Eq a => EqProp (List a) where
  (=-=) = eq

--Chapter Exercises B
--1
j :: Monad m => m (m a) -> m a
j = join

--2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

--3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

--4
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

--5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = do
  r <- f x
  (r:) <$> meh xs f

--6
flipTypes :: Monad m => [m a] -> m [a]
flipTypes = sequence

check :: IO ()
check = do
  quickBatch $ applicative $ ((Second ("bye", [1,2,3] :: [Integer], 'c')) :: (Sum String (String, [Integer], Char)))
  quickBatch $ monad $ ((Second ("bye", [1,2,3] :: [Integer], 'c')) :: (Sum String (String, [Integer], Char)))
  quickBatch $ applicative $ ((Left' ("bye", [1,2,3] :: [Integer], 'c')) :: (PhhhbbtttEither String (String, [Integer], Char)))
  quickBatch $ monad $ ((Left' ("bye", [1,2,3] :: [Integer], 'c')) :: (PhhhbbtttEither String (String, [Integer], Char)))
  quickBatch $ applicative $ Identity (3 :: Integer, "hello", 'c')
  quickBatch $ monad $ Identity (3 :: Integer, "hello", 'c')
  quickBatch $ applicative $ Cons ((5 :: Int),  "bye", 'a') Nil
  quickBatch $ monad $ Cons ((5 :: Int),  "bye", 'a') Nil
