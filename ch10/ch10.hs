-- Exercises Understanding Folds pg 365-367

-- 1 b and c
-- 2 
-- NOTE: I did this for [1..5] instead of [1..3] by accident but same idea just longer and more annoying
-- foldl (flip (*)) 1 [1..5]
-- = foldl (flip (*)) (1*1) [2..5]
-- = foldl (flip (*)) (2*(1*1)) [3..5]
-- = foldl (flip (*)) (3*(2*(1*1))) [4..5]
-- = foldl (flip (*)) (4*(3*(2*(1*1)))) [5]
-- = foldl (flip (*)) (5*(4*(3*(2*(1*1))))) []
-- = (5*(4*(3*(2*(1*1)))))
-- = (5*(4*(3*(2*1))))
-- = (5*(4*(3*2)))
-- = (5*(4*6))
-- = (5*24)
-- = 120
--
-- 3 c
-- 4 a
-- 5a foldr (++) [] ["woot", "WOOT", "woot"] -- no base
-- 5b foldr max '\0' "fear is the little death" -- maybe there's a better more haskell-y way than null char?
-- 5c foldr (&&) True [False, True]
-- 5d foldr (||) False [False, True]
-- 5e foldr ((++) . show) "" [1..5]
-- 5f foldr (flip const) 'a' [1..5] -- this needs to return char since our base is 'a'
-- 5g foldr (flip const) 0 'tacos'
-- 5h foldl const 0 "burritos"
-- 5i foldl const 'z' [1..5]

-- Exercises Scans pg 379

fibsN :: Int -> Integer
fibsN x = fibs !! x
--1
fibs :: [Integer]
fibs = take 20 $ 1 : scanl (+) 1 fibs

--2 
fibsLessThan100 :: [Integer]
fibsLessThan100 = filter (<100) fibs

--3

factorials :: [Integer]
factorials = scanl (*) 1 [2..]

factorial :: Int -> Integer
factorial x = factorials !! x

-- Chapter Exercises

-- Warm up and review pg 379-380

stops = "pbtdkg"
vowels = "aeiou"

--1a

threeTups :: [(Char,Char,Char)]
threeTups = [(a, b, c) | a <- stops, b <- vowels, c <- stops]

--1b

threeTups' :: [(Char,Char,Char)]
threeTups' = [(a, b, c) | a <- stops, b <- vowels, c <- stops, a == 'p']

--1c

nouns = "Actor  Gold    Painting \
         \ Advertisement   Grass   Parrot \
         \ Afternoon   Greece  Pencil \
         \ Airport     Guitar  Piano \
         \ Ambulance   Hair    Pillow" -- credit eslgrammar.com

verbs = "Accept  Guess \
        \ Achieve     Harass \
        \ Add     Hate \
        \ Admire  Hear \
        \ Admit   Help \ 
        \ Adopt   Hit \
        \ Advise  Hope" -- credit eslgrammar.com

myStrings :: Char -> String -> [String] -- I wrote in last chapter exercises
myStrings _ [] = []
myStrings sep s
  | length y == 0 = takeWhile (/= sep) s : (myStrings sep y)
  | otherwise = takeWhile (/= sep) s : (myStrings sep $ tail y)
  where y = dropWhile (/= sep) s

myWords :: String -> [String]
myWords s = myStrings ' ' s

sentenceTups :: [(String,String,String)]
sentenceTups = [(a,b,c) | a <- myWords nouns, b <- myWords verbs, 
                                              c <- myWords nouns]

-- 2

-- get avg number of characters in a group of words like a sentence
seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))

--3

seekritFunc' :: Fractional a => String -> a
seekritFunc' x = (/) (fromIntegral . sum . map length . words $ x) 
                      (fromIntegral . length . words $ x)

-- Rewriting Functions Using Folds pg 381-384

-- I don't think some of these can get more pointfree without using
-- a pointless substitution like a go-function just using a lambda
-- to move the arguments to the right side of the definition. Maybe
-- I'm wrong though.

--1 

myOr :: [Bool] -> Bool
myOr = foldr (||) False

--2 

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

--3 

myElem :: Eq a => a -> [a] -> Bool
myElem i = foldr ((||) . (== i)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' i = myAny (== i)

--4

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

--5

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

--6

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr ((++) . (\x -> if f x then [x] else [])) []

--7

squish :: [[a]] -> [a]
squish = foldr (++) []

--8

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

--9

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

--10

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x y -> if f x y == GT then x else y) (head xs) xs

--11

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\x y -> if f x y == LT then x else y) (head xs) xs
