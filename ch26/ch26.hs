{-# LANGUAGE InstanceSigs #-}

-- Exercsies EitherT pg 1015

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

--1
instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (go f) <$> mea
    where go _  (Left e) = Left e
          go f (Right a) = Right $ f a

--2
instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ pure $ pure x
  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT meab) <*> (EitherT mea) = EitherT $ ((<*>) <$> meab) <*> mea

--3
instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT mea) >>= f = EitherT $ do
                          v <- mea
                          case v of
                            Left e  -> return $ Left e
                            Right a -> runEitherT $ f a

--4

