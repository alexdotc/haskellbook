import Data.List

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n+2) else Nothing

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing

data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

mkPerson' :: Name -> Age -> Either PersonInvalid Person
mkPerson' name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise  = Left AgeTooLow

type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = if age >= 0 then Right age else Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = if name /= "" then Right name else Left [NameEmpty]

mkPerson'' :: Name -> Age -> ValidatePerson Person
mkPerson'' name age = mkPerson''' (nameOkay name) (ageOkay age)

mkPerson''' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson''' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson''' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson''' (Left badName) _             = Left badName
mkPerson''' _              (Left badAge) = Left badAge

-- Chapter Exercises

-- Determine the Kinds pg 480

--1 *
--2 :k a is *, :k f is * -> *

-- String Processing pg 480-481

--1

notThe :: String -> String
notThe = (\s -> if s /= "the" then s else "a")
-- really don't see any reason to use Maybe here, this function doesn't have holes

replaceThe :: String -> String
replaceThe s = foldr ((++) . notThe) [] (intersperse " " $ words s)

--2

isVowel :: Char -> Bool
isVowel = (flip elem) "aeiouAEIOU"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = if length ws < 2 then 0 else go ws
  where 
    go (x:x':xs)
      | length (x:x':xs) == 2 = if target x x' then 1 else 0
      | otherwise = if target x x' then 1 + countTheBeforeVowel (rebuild x' xs)
                    else countTheBeforeVowel (rebuild x' xs)
    ws = words s
    target = (\f -> \s -> isVowel (head s) && f == "the")
    rebuild = (\x -> \xs -> concat $ intersperse " " (x:xs))
--3

countVowels :: String -> Integer
countVowels [] = 0
countVowels (c:s)
  | isVowel c = 1 + countVowels s
  | otherwise = countVowels s

-- Validate the Word pg 481-482

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s
  | (length $ filter isVowel s) > (length $ filter (not . isVowel) s) = Nothing
  | otherwise = Just (Word' s)

-- It's only Natural pg 482-483

data Nat = Zero | Succ Nat deriving (Eq, Show)
type PositiveInteger = Integer

natToInteger :: Nat -> Integer
natToInteger Zero   = 0
natToInteger (Succ n) = succ $ natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0     = Nothing
  | otherwise = Just $ go n
  where
    go 0 = Zero
    go n = Succ (go $ n - 1)

-- Small library for Maybe pg 483-485

--1

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = (not. isJust)

--2

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z _  Nothing  = z
mayybee _ f (Just m) = f m

--3

fromMaybe :: a -> Maybe a -> a
fromMaybe z = mayybee z id

--4

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just (head xs)

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

--5

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (m:ms) = case m of
  Nothing  -> catMaybes ms
  (Just v) -> v : (catMaybes ms)

--6

flipMaybe :: Eq a => [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe xs
  | length xs /= (length $ filter (/= Nothing) xs) = Nothing
  | otherwise = Just (map f xs)
  where f (Just x) = x

flipMaybe' :: [Maybe a] -> Maybe [a]
flipMaybe' = foldr f (Just [])
  where f Nothing _  = Nothing
        f _ Nothing  = Nothing
        f (Just x) (Just xs) = Just (x:xs)

-- Small library for Either pg 485-486

--1

lefts' :: [Either a b] -> [a]
lefts' =  foldr f []
  where f (Right b) = ([] ++)
        f (Left  a) = (a :)

--2

rights' :: [Either a b] -> [b]
rights' =  foldr f []
  where f (Right b) = (b :)
        f (Left  a) = ([] ++)

--3

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' eithers = (lefts' eithers, rights' eithers)

--4

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left a)  = Nothing
eitherMaybe' f (Right b) = Just (f b)

--5

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

--6

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\a -> Nothing) (Just . f)

-- Unfolds pg 486-490

-- Iterate / Unfoldr

--1

myIterate :: (a -> a) -> a -> [a]
myIterate f n = [n] ++ (myIterate f $ f n)

--2

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f n = 
  case f n of
    Nothing -> []
    Just (a, b) -> a : (myUnfoldr f b)

--3

betterIterate :: (a -> a) -> a -> [a]
betterIterate f n = myUnfoldr (\x -> Just (x, f x)) n

-- Binary Tree Unfold

data BinaryTree a = Leaf
                    | Node (BinaryTree a) a (BinaryTree a) 
                    deriving (Eq, Ord, Show)

--1

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f n = 
  case f n of
    Nothing -> Leaf
    Just(x, v, y) -> Node (unfold f x) v (unfold f y)

--2

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where f = (\x -> if x == n then Nothing else Just (succ x, x, succ x))
