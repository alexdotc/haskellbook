import Data.Bool
import Data.Char

-- Exercises EnumFromTo pg 307-308

eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False,True]
eftBool False False = [False]
eftBool True True = [True]
eftBool True False = []

eftOrd :: Ordering -> Ordering -> [Ordering]

eftOrd LT LT = [LT]
eftOrd LT EQ = [LT,EQ]
eftOrd LT GT = [LT,EQ,GT]
eftOrd EQ LT = []
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ,GT]
eftOrd GT GT = [GT]
eftOrd GT _ = []

eftInt :: Int -> Int -> [Int]
eftInt a b
  | a > b = []
  | a == b = [a]
  | a < b = a:(eftInt (succ a) b)

eftChar :: Char -> Char -> [Char]
eftChar a b 
  | a > b = []
  | a == b = [a]
  | a < b = a:(eftChar (succ a) b)

-- Exercises Thy Fearful Symmetry pg 311-312

-- 1
myWords :: String -> [String]
myWords [] = []
myWords s
  | length y == 0 = takeWhile (/= ' ') s : (myWords y)
  | otherwise = takeWhile (/= ' ') s : (myWords $ tail y)
  where y = dropWhile (/= ' ') s

-- 2

myLines :: String -> [String]
myLines [] = []
myLines s
  | length y == 0 = takeWhile (/= '\n') s : (myLines y)
  | otherwise = takeWhile (/= '\n') s : (myLines $ tail y)
  where y = dropWhile (/= '\n') s

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
           \ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

--3 

myStrings :: Char -> String -> [String]
myStrings _ [] = []
myStrings sep s
  | length y == 0 = takeWhile (/= sep) s : (myStrings sep y)
  | otherwise = takeWhile (/= sep) s : (myStrings sep $ tail y)
  where y = dropWhile (/= sep) s

myWords' :: String -> [String]
myWords' s = myStrings ' ' s

myLines' :: String -> [String]
myLines' s = myStrings '\n' s

-- Exercises pg 315-316 Comprehend Thy Lists

--mySqr = [x^2 | x <- [1..10]]

-- 1 [4,16,36,64,100]
-- 2 [(4,64),(4,100),(16,64),(16,100),(36,64),(36,100)] -- I mentally mistakenly added conditions rem 2 x == 0 , rem 2 y == 0 but my comprehension (get it?) is correct
-- 3 [(4,64),(4,100),(16,64),(16,100),(36,64)] -- same as 2

-- Exercises pg 317-318 Square Cube

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

--1

mySqrCube = [(x,y) | x <- mySqr, y <- myCube]

--2 
mySqrCube' = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]

--3
mySqrCube'Count = length [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- Exercises pg 326 Will it blow up?

--1 bottom
--2 return [1]
--3 bottom
--4 return 3
--5 bottom
--6 return [2]
--7 return [] WRONG, bottom
--8 return [1]
--9 return [1,3]
--10 return [1,3] WRONG, bottom

-- Intermission pg 327

--1. NF
--2. WHNF
--3. neither
--4. neither
--5. neither
--6. neither
--7. WHNF

-- Exercises pg333-334 More Bottoms

--1 bottom
--2 return [2]
--3 bottom
--4
itsMystery :: [Char] -> [Bool] -- whether or not each Char in [Char] is a vowel
itsMystery xs = map (\x -> elem x "aeiou") xs
--5a [1,4,9,16,25,36,49,64,81,100]
--5b [1,1,1]
--5c [15,15,15]
--6
negateThree :: (Eq a, Num a) => [a] -> [a]
negateThree xs = map (\x -> bool x (-x) (x==3)) xs

-- Exercises pg 336 Filtering
--1
multiplesOfThree :: (Eq a, Integral a) => [a] -> [a]
multiplesOfThree xs = filter (\x -> x `rem` 3 == 0) xs
--2
howManyMultiplesOfThree :: (Eq a, Integral a) => [a] -> Int
howManyMultiplesOfThree xs = length .  multiplesOfThree $ xs
--3
articles :: [String]
articles = ["the", "an", "a"]

myFilter :: String -> [String]
myFilter s = filter (\s -> not $ elem s articles) . myWords' $ s


-- Exercises pg 338 Zipping Exercises
--1
zip' :: [a] -> [b] -> [(a,b)]
zip' _  [] = []
zip' [] _  = []
zip' (a:as) (b:bs) = (a, b) : zip' as bs
--2
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs
--3
zip'' :: [a] -> [b] -> [(a,b)]
zip'' as bs = zipWith' (,) as bs

-- Chapter Exercises

-- Exercises Data.Char pg 339

--2
toUpperS :: String -> String
toUpperS s = filter isUpper s
--3
capitalize :: String -> String
capitalize "" = ""
capitalize (c:s) = toUpper c : s
--4
shout :: String -> String
shout "" = ""
shout (c:s) = toUpper c : shout s
--5
getTitleLetter :: String -> Char
getTitleLetter "" = '\0'
getTitleLetter s = toUpper $ head s
--6
getTitleLetter' :: String -> Char
getTitleLetter'' :: String -> Char
getTitleLetter' "" = '\0'
getTitleLetter' s = toUpper . head $ s
getTitleLetter'' = toUpper . head

-- Writing your own standard functions pg 342-344

--1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

--2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr . map f $ xs

--3
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem t (x:xs) = (x == t) || myElem t xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' t xs = myAny (== t) xs

--4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

--5
squish :: [[a]] -> [a]
squish [] = []
squish (xs:xss) = xs ++ squish xss

--6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f as = squish . map f $ as

--7
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

--8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy cmp (x:x':xs) = 
  case cmp x x' of
    GT -> myMaximumBy cmp (x:xs)
    _  -> myMaximumBy cmp (x':xs)

--9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (x:[]) = x
myMinimumBy cmp (x:x':xs) = 
  case cmp x x' of
    LT -> myMinimumBy cmp (x:xs)
    _  -> myMinimumBy cmp (x':xs)

--10
myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare
