{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char
import Data.Int

-- Exercises pg 396 Dog Types

--1 type constructor
--2 * -> *
--3 *
--4 Num a => Doggies a
--5 Doggies Integer
--6 Doggies String
--7 both
--8 doge -> DogueDeBordeux doge
--9 DogueDeBordeux String

data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata $ Price 7000
doge = Plane PapuAir (Size 47)

-- Exercises Vehicles pg 400

--1 Vehicle
--2
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar
--3
getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x
--4 exception
--5 ok

-- Exercises Cardinality pg 404

--1 1
--2 3
--3 2^16 or 65536
--4 Int has 2^64, Integer has infinite cardinality
--5 2^8, 8 bits to store

-- Exercises For Example pg 405

data Example = MakeExample deriving Show
data IntExample = MakeIntExample Int

--1 Example, error (not a data constructor)
--2 Show
--3 Make(Int)Example needs an Int value to construct a value of type (Int)Example

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where 
  tooMany = (flip (>)) 42

newtype Goats = Goats Int deriving (Eq, Show, Ord)
newtype Cows  = Cows  Int deriving (Eq, Show, TooMany)
newtype Sheeps = Sheeps Int deriving (Eq, Show)
newtype TMTup = TM (Int, String) deriving (Eq, Show)

instance TooMany Goats where
  tooMany = (flip (>)) (Goats 43)

instance TooMany Sheeps where
  tooMany (Sheeps n) = n > 44

--Exercises Logic Goats pg 410

--1

instance TooMany TMTup where -- don't need FlexibleInstances
  tooMany (TM (n, s)) = n > 45

--2 

instance TooMany (Int, Int) where -- do need FlexibleInstances
  tooMany (f, s) = (f + s) > 86

--3
-- ????

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (f, s) = True

-- Exercises Pity the Bool pg 412

data BigSmall = Big Bool | Small Bool deriving (Eq, Show)

--1 4 [ Big False, Big True, Small False, Small True ]

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

--2 258, Warning

data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth deriving (Eq, Show)
-- cardinality 3

data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)
--cardinality 9


-- records
data Person = Person {  name :: String,
                        age  :: Int
                     } deriving (Eq, Show)

--distributive products

type AuthorName = String

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show

data Author = Author (AuthorName, BookType) 
-- ^non-distributed; a * (b + c) as String * (Fiction + Nonfiction)
data Author' = Fiction' AuthorName | Nonfiction' AuthorName deriving (Eq, Show) 
-- ^"normal form" (pre-term distributed); a * b + a * c as String * Fiction + String * Nonfiction

-- Exercises How Does Your Garden Grow pg 420

--1

data FlowerType = Gardenia | Daisy | Rose | Lilac deriving Show

type Gardener = String

--non-distributed a
data Garden = Garden Gardener FlowerType deriving Show
--distributed, normal
data Garden' = Gardenia' Gardener |  Lilac' Gardener | Daisy' Gardener | Rose' Gardener deriving Show


data GuessWhat = Chickenbutt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst  :: a,
                                         psecond :: b
                                       } deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)
type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo deriving (Eq, Show)
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

trivialValue :: GuessWhat
trivialValue = Chickenbutt

idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool
--type Name = String

person :: Product Name Awesome
person = Product "Simon" True

data Twitter = Twitter deriving (Eq, Show)
data AskFm = AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

data OperatingSystem = GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill 
                     | Mac | Windows deriving (Eq, Show)

data ProgLang = Haskell | Agda | Idris | Purescript deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem,
                               lang :: ProgLang
                             } deriving (Eq, Show)

-- Exercises Programmers pg 430

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill,
                       Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, Purescript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer a b | a <- allOperatingSystems, b <- allLanguages]

newtype Name' = Name' String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving Show

data Farmer = Farmer Name' Acres FarmerType

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec = FarmerRec { name' :: Name', acres :: Acres, 
                             farmerType :: FarmerType} deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _           -> False


data Quantum = Yes | No | Both

convert :: Quantum -> Bool
convert        Yes  = True
convert        No   = True
convert        Both = True

convert'       Yes  = True
convert'       No   = True
convert'       Both = False

convert''      Yes  = True
convert''      No   = False
convert''      Both = False

convert'''     Yes  = False
convert'''     No   = True
convert'''     Both = False

convert''''    Yes  = False
convert''''    No   = True
convert''''    Both = True

convert'''''   Yes  = True
convert'''''   No   = False
convert'''''   Both = True

convert''''''  Yes  = False
convert''''''  No   = False
convert''''''  Both = True

convert''''''' Yes  = False
convert''''''' No   = False
convert''''''' Both = False

-- Exercises The Quad pg 441

data Quad = One | Two | Three | Four deriving (Eq, Show)

--1 4 + 4 = 8
--2 4 * 4 = 16
--3 4 ^ 4 = 256
--4 2 * 2 * 2 = 8
--5 (2 ^ 2) ^ 2 = 16
--6 (4 ^ 4) ^ 2 = 65536

-- Exercises Binary Tree pg 449-451
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) 
                    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
mapOkay = if mapTree (+1) testTree' == mapExpected
             then print "yup okay!"
             else error "test failed!"

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l v r) = [v] ++ (preorder l) ++ (preorder r)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l v r) = (inorder l) ++ [v] ++ (inorder r)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l v r) = (postorder l) ++ (postorder r) ++  [v]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node l v r) = f v (foldTree f (foldTree f z r) l)

-- Chapter Exercises

-- Multiple Choice pg 452

--1 a
--2 c
--3 b
--4 c

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday

-- Exercises As-patterns pg 454-455
-- This sugar would've been helpful for the previous vigenere cipher :-{

--1 

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf subseq@(c:subsubseq) (c':seq) = 
  if c == c' then isSubseqOf subsubseq seq 
  else            isSubseqOf subseq seq

--2
-- oops, I realized after the fact that I was supposed to use as-patterns but
-- oh well, moving on
isLowercaseAlpha :: Char -> Bool
isLowercaseAlpha c = ascii >= 97 && ascii <= 122
                       where ascii = ord c

isUppercaseAlpha :: Char -> Bool
isUppercaseAlpha c = ascii >= 65 && ascii <= 90
                       where ascii = ord c

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = zip ws (map capitalizeWord $ ws)
                      where ws = words s

-- Language Exercises pg 456

--1 

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:s)
  | isLowercaseAlpha c = (chr $ ord c - 32) : s
  | isUppercaseAlpha c = c : s
  | otherwise          = c : capitalizeWord s 


--2 
capitalizeParagraph :: String -> String
capitalizeParagraph s = foldr ((++) . capitalizeWord) [] sentences
  where sentences = (takeWhile (/= '.') s) : [dropWhile (/= '.') s]

-- Phone Exercises pg 456-458
-- TODO -- come back before ch15

-- Hutton's Razor pg 459-460
-- TODO -- come back before ch15
