-- Exercises Grab Bag pg 224

-- 1. all of these are equivalent
-- 2. d

mTh :: Num a => a -> a -> a -> a
mTh = \x -> \y -> \z -> x*y*z

--3a

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

--3b

addFive = \x -> \y -> (if x > y then y else x) + 5

--3c

mflip f x y = f y x
