{-# LANGUAGE InstanceSigs #-}

newtype Identity a = Identity { runIdentity :: a }
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- Exercise Compose Applicative pg 989

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose $ pure . pure $ x
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ (fmap (<*>) f) <*> a

-- Compose [Just (+1), Just (+2), Just (+3)] <*> Compose [Just 3, Just 2, Just 1]
-- -- Compose $ [(<*>) Just (+1), (<*>) Just (+2), (<*>) Just (+3)] <*> [Just 3, Just 2, Just 1]
-- -- Compose $ [Just (+1) <*> Just 3, Just (+1) <*> Just 3, ... , Just (+3) <*> Just 1]
-- Compose [Just 4, Just 3, Just 2, Just 5, Just 4, Just 3, Just 6, Just 5, Just 4]

-- Exercise Foldable/Traversable Compose Instances 
-- TODO come back after ch21, review 20

--1

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = foldMap (id . foldMap f) fga

-- Exercise Bifunctor pg 992

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  
  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- 1
data Deux a b = Deux a b deriving (Eq, Show)

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

--2
data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const $ f a -- why is it called Const? that's misleading... 

--3
data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

--4
data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

--5
data SemiDrei a b c = SemiDrei a
 
instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

--6
data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

--7
instance Bifunctor Either where
  bimap f g (Left a) = Left $ f a
  bimap f g (Right a) = Right $ g a

newtype IdentityT m a = IdentityT { runIdentityT :: m a }

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT ma) = IdentityT $ f <$> ma

instance Applicative m => Applicative (IdentityT m) where
  pure x = IdentityT $ pure x
  (IdentityT fab) <*> (IdentityT fa) = IdentityT $ fab <*> fa

instance Monad m => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
