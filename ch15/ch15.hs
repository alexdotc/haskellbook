import Control.Monad
import Data.Monoid
import Test.QuickCheck

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

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
