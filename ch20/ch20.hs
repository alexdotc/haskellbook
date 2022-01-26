import Data.Monoid

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
