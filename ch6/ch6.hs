import Data.List

-- Exercises pg 206
--
-- 1 c
-- 2 b
-- 3 a
-- 4 c
-- 5 a
--
-- Exercises pg 207
--
-- 1 no, no instance of Show Person
-- 2 no, no instance of Eq Mood

data Mood = Blah | Woot deriving (Show, Eq)

settleDown x = if Woot == x then Blah else x

-- 3a values of type Mood
-- 3b no data constructor for type Mood : 9 TODO right but wrong with error
-- 3c No instance of Ord Mood, fails
-- 4 No because Sentence "dogs" "drool" is not a valid data constructor

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- TODO WRONG, it will typecheck. But s1 will be of type Object -> Sentence and so it can't be shown (since (->) has no instance of Show
--
-- Exercises pg 209 (Given a ... what can we do?)

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- 1 no, these data constructors don't match what's actually given
-- 2 yes
truth = Papu (Rocks "chomskydoz") (Yeah True)
-- 3 yes
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'
-- 4 no, no instance of Ord Papu

-- Exercises pg 209-211 Match teh types
--
-- 1. yes, but maybe it won't Show WRONG because the compiler tries to infer Num a => a in i = 1 when it sees the data constructor 1
i :: Num a => a
i = 1

-- 2. no, because the compiler will need to infer at least Fractional a => a for the data constructor 1.0
f :: Float
f = 1.0

-- 3. yes
-- 4. yes
-- 5. yes
-- 6. yes
-- 7. no, because this always returns Int and Int is a cocnrete type vs the fully polymorphic type a
-- 8. no, for the same reason as 8
-- 9. yes
-- 10. yes
young :: [Int] -> Int
young xs = head (sort xs)
-- 11. no, because mySort can only take [Char] and signifier can now take [a]
mySort :: [Char] -> [Char]
mySort = sort

--signifier :: Ord a => [a] -> a
signifier :: [Char] -> Char
signifier xs = head (mySort xs)

-- Exercises pg 211

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f a b = (f b) + fromInteger a -- TODO the fact that (f b) should have type Num b => b makes it kind of confusing that I have to use fromInteger a when I can normally add an Integer and Num constrained Num b => b. OH I see. it's that I've declared b to be constrained by Num b, and it's not that I can add that b and an Integer, it's that the result of addition would be Integer, whereas I've declared the return type (fully applied type) of arith is Num b => b. So I've accepted that b can be float or anything else with an instance Num.


