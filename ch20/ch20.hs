import Data.Monoid
import Data.Bool

-- Exercises Library Functions Page 835

--1

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

--2

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

--3

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = getAny . foldMap (Any . (==x))

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr ((||) . (==x)) False

--4

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr (f . Just) Nothing
  where
    f (Just x) Nothing = Just x
    f (Just x) (Just y) = Just $ min x y

--5

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr (f . Just) Nothing
  where
    f (Just x) Nothing = Just x
    f (Just x) (Just y) = Just $ max x y

--6

null :: (Foldable t) => t a -> Bool
null = foldr (\a -> \b -> False) True

--7

length :: (Foldable t) => t a -> Int
length = foldr ((+) . const 1) 0

--8

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

--9

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

--10

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty

-- Chapter Exercises pg 836

--1

data Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap f (Constant x) = f x

--2

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

--3

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

--4

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = f b <> f c

--5

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b c d) = f b <> f c <> f d

--Thinking Cap Time

filterF :: (Applicative f, Foldable t, Monoid (f a))
           => (a -> Bool) -> t a -> f a
filterF g = foldMap (\x -> bool mempty (pure x) (g x))
