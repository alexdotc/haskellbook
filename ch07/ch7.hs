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

funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

-- Exercises Case Practic pg 238

functionC x y = 
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 n =
  case even n of
    True -> n+2
    False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnLast' :: a -> (b -> (c -> (d -> d))) -- same as returnLast
returnLast' _ _ _ d = d

-- returnBroke :: (((a -> b) -> c) -> d) -> d -- no
-- returnBroke _ _ _ d = d error. only 1 arg which is an HOF, not 4

-- returnBroke' :: (((a -> b) -> c) -> d) -> d
-- returnBroke f = IMPOSSIBLE TO DEFINE! 

returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f a c = f a -- return the value f applied to a, which has type b

data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' = case compare e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee\
                    \ is the boss"
    LT -> (flip reportBoss) e e'

-- Exercises pg 247

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

--2  return 11
--3  return 22
--4  return 21
--5  return 12
--6  return 11
--7  return 21
--8  return 21
--9  return 22
--10 return 31
--11 return 23

myAbs :: Integer -> Integer
myAbs x
    | x < 0 = (-x)
    | otherwise = x -- otherwise = True (alias)

-- Exercises pg 252 

--1  always F since the first guard always evaluates to True

avgGrade :: (Fractional a, Ord a ) => a -> Char
avgGrade x
    | otherwise = 'F'
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.59 = 'D'
    where y = x / 100

--2 It still type checks but no , of course it doesn't work the same way. You need descending grades for this to work

pal xs
    | xs == reverse xs = True
    | otherwise = False
--3 b
--4 whatever reverse can take, so probably String. NO actually list of anything. Ok. But it at least needs instance of Eq too.
--5 pal :: Eq a => [a] -> Bool

numbers x
    | x < 0  = -1
    | x == 0 = 0
    | x > 0  = 1

--6 c
--7 Num a => a
--8 numbers :: (Num a, Num b) => a -> b ALSO NEED Ord a => (7 and 8) :)


-- Chapter Exercises pg 262

--1 d
--2 b
--3 d
--4 b
--5 a

-- Exercises pg 263

--1a

-- ones and 10s
tensDigit :: Integral a => a -> (a, a)
tensDigit x = (d,c)
              where b = divMod x 10
                    c = snd b
                    d = flip(mod) 10 . fst $ b

--1b no, not the way I did it with the extra stuff. could just only return 10s and then yes
--1c any place digit (10, 100th, 100th etc)

dsDigit :: Integral a => a -> a -> a
dsDigit x i = flip(mod) i . div x $ i

-- 2

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y b =  
  case b of
    False -> x
    True -> y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y b
  | b == False = x
  | otherwise  = y

foldBoolMatch :: a -> a -> Bool -> a
foldBoolMatch x _ False = x
foldBoolMatch _ y True  = y

foldBoolIf :: a -> a -> Bool -> a
foldBoolIf x y b = if b then y else x

-- 3

g :: (a -> b) -> (a, c) -> (b, c)
g f (x,y) = (f x, y)

-- 4,5

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read . show $ a

-- 6 TODO COME BACK TO THIS ONE

--roundTrip' :: (Show a, Read b) => a -> b
--roundTrip' a = read :: String -> a. show $ a
