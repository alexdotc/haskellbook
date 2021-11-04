{-# LANGUAGE NoMonomorphismRestriction #-}
-- Exercises: Type Matching pg 125

--a, c
--b, d
--c, b
--d. a
--e, e

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

-- Exercises: type arguments pg 134

--1. a
--2. d
--3. c WRONG, its d
--4. c
--5. a
--6. e
--7. e WRONG, its d
--8. a
--9. c

-- Exercises:Parametricity pg 140

myPPfunction :: a -> a
myPPfunction a = if True then a else a

myGGfunction :: a -> a -> a
myGGfunction a b = a
--or myGGfunction a b = b

myHHfunction :: a -> b -> a
myHHfunction a b = a

-- Exercises: Apply Yourself pg 145


--1. myConcat :: String -> String (can infer from "yo")
--2. myMult :: Fractional a => a -> a
--3. myTake :: Int -> String
--4. myCom :: Int -> Bool
--5. myAlph :: Char -> Bool

myConcat x = x ++ " yo"
myMult x = (x/3) * 5
myTake x = take x "hey you"
myCom x = x > (length [1..10])
myAlph x = x < 'z'

t1 :: Num a => a -> a -> a
t1 a b = a + b
t2 c d = t1 c (d :: Integer)

-- Chapter Exercises

-- pg 148

--1. c
--2. a
--3. a
--4. c

-- pg 149

--1a. Num a => a , 54
--1b. Num a => (a, String) , (0, "doge")
--1c. (Integer, String), (0, "doge")
--1d. Bool, False
--1e. Int, 5
--1f. Bool, False
--2.  Num a => a
--3.  Num a => a -> a
--4.  Fractional a => a
--5.  String

a = (*9) 6
b = head [(0,"doge"),(1,"kitteh")]
c3 = head [(0 :: Integer,"doge"),(1,"kitteh")]
d = if False then True else False
e = length [1,2,3,4,5]
f = (length [1,2,3,4]) > (length "TACOCAT")

x = 5
y = x + 5
w = y * 10

z y = y * 10

notf = 4 / y

g = "Julie"
h = " <3 "
notc = "Haskell"
i' = g ++ h ++ notc

-- pg 150

--1. no. can't apply terminal bigNum

bigNum = (^) 5
wahoo = bigNum $ 10 -- 5^10

--2. yes

x2 = print
y2 = print "woohoo!"
z2 = x2 "hello world"

--3. no. can't apply terminal 5

a2 = (+)
b2 = 5
c2 = a2 10 -- return partial app (10+)
d2 = c2 200 -- return 210

--4. no. there's just no such thing as c.

-- pg 151

--2. fully polymorhpic (zed), concrete (Zed), concrete (Blah)
--3. fully polymorphic (a), constrained polymorphic of Enum (b), concrete (C)
--4. fully polymorphic (f), fully polymorphic (g), concrete (C)

-- pg 151

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

-- pg 152

i :: a -> a
i x = x

c :: a -> b -> a
c a b = a


-- yes, same
c'' :: b -> a -> b
c'' a b = a

c' :: a -> b -> b
c' a b = b

r :: [a] -> [a]
r = undefined -- TODO

co :: (b -> c) -> (a -> b) -> a -> c
co a b c = a (b c)

a' :: (a -> c) -> a -> a
a' a b = b

a'' :: (a -> b) -> a -> b
a'' a b = a b

-- pg 154

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y) then fstString x else sndString y
       where x = "Singin"
             y = "Somewhere"

sing' = if (x < y) then fstString x else sndString y
       where x = "Singin"
             y = "Somewhere"

main :: IO ()
main = do
  print (1 + 2)
  print 10
  print (negate (-1))
  print ((+) 0 blah)
  where blah = negate 1

-- pg 156

f''' :: Int -> String
f''' = undefined
g''' :: String -> Char
g''' = undefined
h''' :: Int -> Char
h''' a = g''' $ f''' a

data A
data B
data C

q'' :: A -> B
q'' = undefined
w'' :: B -> C
w'' = undefined
e'' :: A -> C
e'' a = w'' (q'' a)

data X
data Y
data Z

xz :: X -> Z
xz = undefined
yz :: Y -> Z
yz = undefined
xform :: (X, Y) -> (Z, Z)
xform (a, b) = (xz a, yz b)

munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge a b c = fst(b (a c))
