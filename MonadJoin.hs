module MonadJoin where

import Prelude hiding (sequence)

import Control.Monad (join)
import Control.Applicative
import Data.Functor.Compose
import Data.Traversable
import Data.Foldable

-- Monad definable by join instead of bind.
class (Applicative m) => Monad' m where
  return' :: a -> m a
  bind' :: m a -> (a -> m b) -> m b
  join' :: m (m a) -> m a
  ap' :: m (a -> b) -> m a -> m b
  
  bind' ma f = join' (fmap f ma)
  join' mma = bind' mma id
  ap' mf ma = bind' mf $ \f -> fmap f ma

-- allows easy conversion from Monad to Monad'
instance Monad m => Monad' (WrappedMonad m) where
  return' = return
  bind' = (>>=)

instance (Monad' m, Monad' n, Traversable n) => Monad' (Compose m n) where
  return' = Compose . return' . return'

  --join' :: Compose m n (Compose m n a) -> Compose m n a
  join' = Compose . (fmap join') . join' . (fmap sequenceA) . getCompose . (fmap getCompose)

-- are there any other Monad (Compose m n) instances out there?
instance (Functor m, Monad m, Monad n, Traversable n) => Monad (Compose m n) where
  return = Compose . return . return
  ma >>= f = join_ (fmap f ma)
    where join_ = Compose . (fmap join) . join . (fmap sequence) . getCompose . (fmap getCompose)

instance Monad' Maybe where
  return' = Just
  join' (Just j) = j
  join' Nothing = Nothing

instance Monad' (Either e) where
  return' = Right
  join' (Right r) = r
  join' (Left e) = Left e

-- anything Foldable and a Monoid(1) is a Monad
instance Monad' [] where
  return' = pure
  join' = fold
  bind' = flip foldMap

