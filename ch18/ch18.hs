import Control.Monad (join, liftM2)
import Test.QuickCheck

-- Exercise Bind in terms of fmap and join pg 750

bind :: Monad m => (a -> m b) -> m a -> m b 
bind f = join . fmap f

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' = putStrLn "blah" >> putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' = putStrLn "blah" *> putStrLn "another thing"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' = putStrLn "name pls:" >> getLine >>= \name ->
                          putStrLn ("y helo thar: " ++ name)

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x then [x*x, x*x] else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  [x]

-- Exercise Either Monad

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second z) = Second $ f z
  fmap f (First z) = First z

instance Applicative (Sum a) where
  pure x = Second x
  First x <*> _ = First x
  _ <*> First x = First x
  Second f <*> Second y = Second $ f y

instance Monad (Sum a) where
  return = pure
  (First b) >>= _  = First b
  (Second a) >>= k = k a

-- Chapter Exercises Monad instances pg 788

--1

data Nope a = NopeDotJpg

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  _ <*> _ = NopeDotJpg
  pure _  = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

--2

data PhhhbbtttEither b a = Left' a | Right' b

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' z) = Left' $ f z
  fmap f (Right' z) = Right' z

instance Applicative (PhhhbbtttEither b) where
  pure x = Left' x
  Right' x <*> _ = Right' x
  _ <*> Right' x = Right' x
  Left' f <*> Left' y = Left' $ f y

instance Monad (PhhhbbtttEither b) where
  return = pure
  (Right' b) >>= _  = Right' b
  (Left' a) >>= k = k a

--3

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

instance Monad Identity where
  return = pure
  (Identity x) >>= f = f x

--4

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

instance Monad List where
  return = pure
  (Cons x xs) >>= f = join $ f <$> (Cons x xs)
  _ >>= _ = Nil

-- Chapter Exercises write functions pg 789

--1

j :: Monad m => m (m a) -> m a
j = join

--2

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = (<$>)

--3

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

--4

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

--5

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = []
meh (x:xs) f = x >>= f (meh xs f)
