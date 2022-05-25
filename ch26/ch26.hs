{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans

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
swapEither :: Either e a -> Either a e
swapEither (Left x)  = Right x
swapEither (Right x) = Left x

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ swapEither <$> mea

--5
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mab) = mab >>= (go f g)
  where go f _ (Left x)  = f x
        go _ g (Right x) = g x

-- Exercises StateT pg 1019

--1
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT mas) = StateT $ (fmap . fmap) (go f) mas
    where go f (a, s) = (f a, s)

--2
instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x,s)
  (StateT smabs) <*> (StateT smas) = StateT $ \s -> do
                                                (f, s') <- smabs s
                                                (a, s'') <- smas s'
                                                return (f a, s'')

--3
instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT smas) >>= f = StateT $ \s -> do
                                    (a, s') <- smas s
                                    runStateT (f a) s'

-- Exercises Wrapping it Up pg 1026

embedded' :: MaybeT (ExceptT String IO) Int
embedded' = MaybeT (ExceptT $ return (Right (Just 1)))

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT (ExceptT (ReaderT (return <$> (const (Right (Just 1))))))

-- Exercises Lift More pg 1037

--1
instance MonadTrans (EitherT e) where
  lift = EitherT . (<$>) Right

--2
instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift = \ma -> StateT $ (\s -> ma >>= (\a -> return (a, s)))

-- Exercises Some Instances pg 1043

newtype MaybeT2 m a = MaybeT2 (MaybeT m a) deriving (Eq, Show, Functor, Applicative, Monad)
newtype ReaderT2 r m a = ReaderT2 (ReaderT r m a) deriving (Functor, Applicative, Monad)
newtype StateT2 s m a = StateT2 (StateT s m a) deriving (Functor, Applicative, Monad)

--1
instance (MonadIO m) => MonadIO (MaybeT2 m) where
  liftIO :: IO a -> MaybeT2 m a
  liftIO = MaybeT2 . lift . liftIO

--2
instance (MonadIO m) => MonadIO (ReaderT2 r m) where
  liftIO :: IO a -> ReaderT2 r m a
  liftIO = ReaderT2 . lift . liftIO

--3
instance (MonadIO m) => MonadIO (StateT2 s m) where
  liftIO :: IO a -> StateT2 s m a
  liftIO = StateT2 . lift . liftIO
