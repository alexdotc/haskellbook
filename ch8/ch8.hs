import Data.List

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n-1) f $ b

-- Exercise Intermission pg 282-283

-- applyTimes 5 (+1) 5
-- = (+1) . applyTimes 4 (+1) $ 5
-- = (+1) . (+1) . applyTimes 3 (+1) $ 5
-- = (+1) . (+1) . (+1) . applyTimes 2 (+1) $ 5
-- = (+1) . (+1) . (+1) . (+1) applyTimes 1 (+1) $ 5
-- = (+1) . (+1) . (+1) . (+1) . (+1) . applyTimes 0 (+1) $ 5
-- = (+1) . (+1) . (+1) . (+1) . (+1) . 5
-- = (+1) . (+1) . (+1) . (+1) . 6
-- = (+1) . (+1) . (+1) . 7
-- = (+1) . (+1) . 8
-- = (+1) . 9
-- = 10

excf :: Bool -> Int
excf True = error "blah"
excf False = 0

excf' :: Bool -> Maybe Int
excf' False = Just 0
excf' _ = Nothing

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

-- Exercises pg 294

--1 d
--2 b
--3 d
--4 b

-- Exercises pg 295

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"

--1 "woops mrow woohoo!"
--2 "1 mrow haha"
--3 "woops mrow 2 mrow haha"
--4 "woops mrow blue mrow haha"
--5 "pink mrow haha mrow green mrow woops mrow blue"
--6 "are mrow Pugs mrow awesome"

--Exercsies Recursion pg 295-296
--1

-- nah don't feel like it

--2

rsum :: (Eq a, Num a) => a -> a
rsum 1 = 1
rsum n = n + rsum (n-1)

--3 

rmult :: Integral a => a -> a -> a
rmult a 1 = a
rmult a b = a + rmult a (b-1)

-- Fixing Dividedby pg 296

dividedBy :: Integral a => a -> a -> Maybe (a, a)
dividedBy _ 0 = Nothing
dividedBy num denom = go num denom 0 (if num*denom < 0 then True else False)
  where go n  d count isNegative
         | isNegative && abs n < d = Just (negate count, n)
         | abs n < d = Just (count, n)
         | (n-d) > n && (d-n) > d = go (-n) (-d) count isNegative
         | (n-d) > n = go (n+d) (-d) (count + 1) isNegative
         | (d-n) > d = go (n+d) d (count + 1) isNegative
         | otherwise = go (n-d) d (count + 1) isNegative

-- Exercsies McCarthy 91 pg 297-298

mc91 :: Integral a => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11


-- Exercises Numbers into Words pg 297

digitToWord :: Int -> String
digitToWord d =
  case d of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    _ -> wordNumber d

digit :: Int -> [Int]
digits n
    | n < 10 = [n]
    | otherwise = (digits $ n `div` 10) ++ [n `mod` 10]

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" . map digitToWord . digits $ n
