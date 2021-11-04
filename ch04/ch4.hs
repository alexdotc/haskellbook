-- Mood Swing pg 89

--1. Type constructor is Mood
--2. Blah or Woot
--3. The type signature uses data constructors (values) instead of type constructor (types)

--4.

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

-- Find the Mistakes pg 101

--1. needs capitalized "true" as valid data constructor for second arg of &&
--2. not a boolean arg to not
--3. no mistake
--4. needs quote for strings
--5. trying to concat list of Num with list of Char

snd' :: (a, b) -> b
snd' (b,a) = a

-- Chpter exercises pg 110

--  1. 

-- length :: [a] -> Integer

-- 2a. 5
-- 2b. 3
-- 2c. 2
-- 2d. 5

-- 3. Doesn't work since (/) is Fractional a => a -> a -> a (i.e. a is a Fractional) and length returns Integral type

-- 4.

fixed4 = 6 `div` length [1,2,3] -- integer division

-- 5. Bool, True
-- 6. Bool, False
-- 7. True, Error (list of multiple types), 5, False, Error (Int argument for &&)
-- 8.

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 9.

myAbs :: Integer -> Integer
myAbs x = if x < 0 then -x else x

-- 10.

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((snd (a,b), snd(c,d)), (fst(a,b), fst(c,d)))

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- Correcting Syntax pg 112

-- 1. 2 issues: cannot have capitalized function identifier, 'x' needs to be in backticks infix or with nothing and prefix

x = (+)
g xs = w `x` 1
     where w = length xs

-- 2. 

y y = y

-- 3.

h (a, b) = a

-- Match function names to types pg 112

-- 1. c
-- 2. b
-- 3. a
-- 4. d
