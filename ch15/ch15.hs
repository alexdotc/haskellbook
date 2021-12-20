import Control.Monad
import Data.Monoid
import Test.QuickCheck hiding (Success, Failure)

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

-- Exercise Optional Monoid pg 598

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (Only x) <> (Only y) = Only $ x <> y
  Nada <> x = x
  x <> Nada = x

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

-- Exercise madlibbinBetter' pg 605


madlibbin'Better :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin'Better e adv noun adj = mconcat xs
  where xs = [e, "! he said ", adv, " as he jumped into his car ",
              noun, " and drove off with his ", adj, " wife."]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = a <> (b<>c) == (a<>b) <> c

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

main' :: IO ()
main' = do
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity
  quickCheck (ma :: BullMappend)
  quickCheck (mli :: Bull -> Bool)
  quickCheck (mri :: Bull -> Bool)

-- Exercise Maybe Another Monoid pg 611

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
  First' (Only a) <> _ = First' $ Only a
  First' Nada <> First' (Only a) = First' $ Only a
  First' Nada <> First' Nada = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada

fgen :: Arbitrary a => Gen (First' a)
fgen = do
  a <- arbitrary
  return $ First' $ Only a

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = frequency [(10, fgen), (1, return (First' Nada))]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

main'' :: IO ()
main'' = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

-- Chapter Exercises Semigroup pg 618

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc) -- 1
  quickCheck (semigroupAssoc :: IdentityAssoc) -- 2
  quickCheck (semigroupAssoc :: TwoAssoc) -- 3
  quickCheck (semigroupAssoc :: ThreeAssoc) -- 4
  quickCheck (semigroupAssoc :: FourAssoc) -- 5
  quickCheck (semigroupAssoc :: BoolConjAssoc) -- 6
  quickCheck (semigroupAssoc :: BoolDisjAssoc) -- 7
  quickCheck (semigroupAssoc :: OrAssoc) -- 8
--  quickCheck (semigroupAssoc :: CombineAssoc) -- 9
--  quickCheck (semigroupAssoc :: CompAssoc) -- 10
  quickCheck (semigroupAssoc :: ValidationAssoc) -- 11

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c )) == ((a <> b) <> c)

--1

data Trivial = Trivial deriving (Eq, Show)
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

--2

newtype Identity a = Identity a deriving (Eq, Show)
type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity $ x <> y

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

--3 

data Two a b = Two a b deriving (Eq, Show)
type TwoAssoc = Two String (Sum Int) -> Two String (Sum Int) 
                -> Two String (Sum Int) -> Bool

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

--4

data Three a b c = Three a b c deriving (Eq, Show)
type ThreeAssoc = Three String (Sum Int) (Product Int)
                  -> Three String (Sum Int) (Product Int)
                  -> Three String (Sum Int) (Product Int)
                  -> Bool

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c)
  where
   (Three a b c) <> (Three d e f) = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c)
  where
   arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

--5 

data Four a b c d = Four a b c d deriving (Eq, Show)
type FourAssoc = Four String (Sum Int) (Product Int) String
                  -> Four String (Sum Int) (Product Int) String
                  -> Four String (Sum Int) (Product Int) String
                  -> Bool

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d)
  where
   (Four a b c x) <> (Four d e f y) = Four (a <> d) (b <> e) (c <> f) (x <> y)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d)
  where
   arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    w <- arbitrary
    return $ Four x y z w

--6

newtype BoolConj = BoolConj Bool deriving (Eq, Show)
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _                             = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = frequency [(1, return $ BoolConj True), (1, return $ BoolConj False)]

--7

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _                               = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = frequency [(1, return $ BoolDisj True), (1, return $ BoolDisj False)]

--8

data Or a b = Fst a | Snd b deriving (Eq, Show)
type OrAssoc = Or String (Sum Int) -> Or String (Sum Int) 
                -> Or String (Sum Int) -> Bool

instance Semigroup (Or a b) where
  (Snd x) <> _ = Snd x
  _ <> (Snd x) = Snd x
  x <> y       = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return $ Fst x), (1, return $ Snd y)]

--9

newtype Combine a b = Combine { unCombine :: (a -> b) }
type CombineAssoc = Combine (Sum Int) (Sum Int) -> Combine (Sum Int) (Sum Int)
                    -> Combine (Sum Int) (Sum Int) -> Bool

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ \x -> f x <> g x

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

--10

newtype Comp a = Comp { unComp :: (a -> a) }
type CompAssoc = Comp (Sum Int) -> Comp (Sum Int)
                    -> Comp (Sum Int) -> Bool -- ?

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp (g . f)

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

--11

data Validation a b = Failure a | Success b deriving (Eq, Show)
type ValidationAssoc = Validation String Int -> Validation String Int
                       -> Validation String Int -> Bool

instance Semigroup a => Semigroup (Validation a b) where
  Failure x <> Failure y = Failure $ x <> y
  Success x <> y         = Success x
  x <> y                 = y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(3, return $ Success x), (1, return $ Failure y)]
