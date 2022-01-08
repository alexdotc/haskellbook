{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr
import Test.QuickCheck
import Test.QuickCheck.Function

--Exercises Be Kind pg 634

--1 *
--2 both are * -> *
--3 * -> * -> *

-- double fmap pg 650


-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- fmap :: Functor g => (x -> y) -> g x -> g y
-- fmap :: Functor f => (m -> n) -> f m -> f n
--
-- visually parenthesize to help denote a and b for clarity
--
-- (.) :: (Functor f, Functor g) => ((x -> y) -> (g x -> g y))
--                                  -> ((m -> n) -> (f m -> f n)) 
--                                  -> ((m -> n) -> (g x -> g y))
--
-- replace left fmap's (x -> y) with b from right fmap in (.)
--
-- (.) :: (Functor f, Functor g) => ((f m -> f n) -> (g (f m) -> g (f n)))
--                                  -> ((m -> n) -> (f m -> f n)) 
--                                  -> (m -> n) -> (g (f m) -> g (f n))
--
-- apply (.)
--
-- (fmap . fmap) :: (Functor f, Functor g) => (m -> n) -> g (f m) -> g (f n)

-- Exercises Heavy Lifting pg 656

--1 
a = fmap (+1) $ read "[1]" :: [Int]

--2
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

--3
c = fmap (*2) (\x -> x - 2)

--4
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

--5
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
--      changed = fmap read (fmap ("123" ++) (fmap show ioi))
        changed = fmap (read . ("123" ++) . show) ioi
    in fmap (*3) changed

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => f a -> (a -> b) -> (b -> c) -> Bool
functorCompose x f g = (fmap g (fmap f x)) == (fmap (g . f) x)


--using QuickCheck to generate random functions
functorCompose' :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap g . fmap f $ x) == fmap (g . f) x

-- Exercises Instances of Func pg 663

main :: IO ()
main = do
  quickCheck (functorIdentity :: IdentityIdentity) --1
  quickCheck (functorCompose' :: IdentityCompose) --1
  quickCheck (functorIdentity :: PairIdentity) -- 2
  quickCheck (functorCompose' :: PairCompose) --2
  quickCheck (functorIdentity :: TwoIdentity) -- 3
  quickCheck (functorCompose' :: TwoCompose) -- 3
  quickCheck (functorIdentity :: ThreeIdentity) -- 4
  quickCheck (functorCompose' :: ThreeCompose) -- 4
  quickCheck (functorIdentity :: Three'Identity) -- 5
  quickCheck (functorCompose' :: Three'Compose) -- 5
  quickCheck (functorIdentity :: FourIdentity) -- 6
  quickCheck (functorCompose' :: FourCompose) -- 6
  quickCheck (functorIdentity :: Four'Identity) -- 7
  quickCheck (functorCompose' :: Four'Compose) -- 7
  

--1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

type IdentityIdentity = Identity Int -> Bool
type IdentityCompose  = Identity Int -> Fun Int String -> Fun String Char -> Bool

--2

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

type PairIdentity = Pair Int -> Bool
type PairCompose = Pair Int -> Fun Int String -> Fun String [Int] -> Bool

--3

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x $ f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

type TwoIdentity = Two [Int] String -> Bool
type TwoCompose  = Two [Int] String -> Fun String Int -> Fun Int String -> Bool

--4

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y $ f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c)
  where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return $ Three x y z

type ThreeIdentity = Three Char [Int] String -> Bool
type ThreeCompose  = Three Char [Int] String -> Fun String Int -> Fun Int String -> Bool

--5

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b)
  where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return $ Three' x y z

type Three'Identity = Three' Char [Int] -> Bool
type Three'Compose  = Three' Char [Int] -> Fun [Int] Int -> Fun Int String -> Bool

--6

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y $ f z

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) 
  => Arbitrary (Four a b c d) where
    arbitrary = do
      w <- arbitrary
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return $ Four w x y z

type FourIdentity = Four Char [Int] String Int -> Bool
type FourCompose  = Four Char [Int] String Int -> Fun Int Int -> Fun Int String -> Bool

--7

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y $ f z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four' w x y z

type Four'Identity = Four' Char Int -> Bool
type Four'Compose  = Four' Char Int -> Fun Int Int -> Fun Int String -> Bool

--8
-- No, Trivial is a type constant with kind * ( not * -> * )

-- Exercise Possibly pg 666

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers $ f a
  fmap _ _           = LolNope

-- Short Exercise pg 668

--1

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second $ f b
  fmap _ (First a)  = First a

--2 First is structurally part of the Functor itself due to kind requirements. Only the last applied type argument (the one that reduces kind * -> * to kind *) is not part of the Functor.

type Nat f g = forall a . f a -> g a

data Tuple a b = Tuple a b deriving (Eq, Show)

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

-- Chapter Exercises pg 679

--1 
-- no valid functor for this since it's kind *

--2

data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' $ f x
  fmap f (True'  x) = True'  $ f x

--3

data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap f (Truish x) = Truish $ f x
  fmap _ _          = Falsish

--4
newtype Mu f = InF { outF :: f (Mu f) }
-- no valid functor since this is kind (* -> *) -> *
-- and we can only apply away to *

--5
-- no valid functor for this since it's kind *

--Chapter Exercises Rearrange Type Constructor pg 680

--1

--data Sum' a b = First' a | Second' b
data Sum' b a = First' a | Second' b

instance Functor (Sum' e) where
  fmap f (First' a) = First' $ f a
  fmap f (Second' b) = Second' b

--2

--data Company a b c = DeepBlue a c | Something b
data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something $ f b
  fmap _ (DeepBlue a c) = DeepBlue a c

--3

--data More a b = L a b a | R b a b deriving (Eq, Show)
data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Chapter Exercises Write Functor Instances pg 681

--1

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor $ f b
  fmap _ (Desk  a) = Desk a
  fmap _ _         = Finance

--2

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

--3

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K $ f a)

--4

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst a) = GoatyConst $ f a

--5

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut g) = LiftItOut (fmap f g)

--6

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa g h) = DaWrappa (fmap f g) (fmap f h)

--7

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething a b) = IgnoringSomething a (fmap f b)

--8

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

--9

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap f (Cons a as) = Cons (f a) (fmap f as)
  fmap _ _           = Nil

--10

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) 
                                                 (GoatLord a) 
                                                 (GoatLord a)
                  deriving (Eq, Show)

instance Functor GoatLord where
  fmap f (MoreGoats x y z) = MoreGoats (a x) (a y) (a z)
    where a = fmap f
  fmap f (OneGoat x) = OneGoat $ f x
  fmap _ _           = NoGoat

--11

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  -- also fmap f (Read g) = Read $ f . g
  fmap f (Read g) = Read $ fmap f g
  fmap f (Print s a) = Print s $ f a
  fmap _ _ = Halt
