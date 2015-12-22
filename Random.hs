{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Random where

import Control.Lens

import Control.Monad.Random (Rand, fromList)
import System.Random (RandomGen)

import Data.Foldable
import Data.Traversable
import Control.Applicative (Applicative, pure, (<*>), (<$>))
import Control.Monad

import Data.Functor.Compose
import Data.Functor.Classes

import Control.Monad.Free
import Control.Monad.Free.Church

import NumericPrelude
import qualified Algebra.Ring as Ring
import qualified Algebra.Module as Module
import qualified Algebra.Field as Field
import qualified Prelude

import MonadJoin
import Tree

class (Field.C w, Monad m) => MonadDiscrete w m | m -> w where
  sample :: [(a, w)] -> m a
  uniform :: [a] -> m a
  uniform as = sample [(a, w) | a <- as]
    where w = one / fromIntegral (length as)

instance RandomGen g => MonadDiscrete Prelude.Rational (Rand g) where
  sample = fromList

instance MonadDiscrete Prelude.Rational IO where
  sample = fromList

newtype Weighted w a = Weighted { asPair :: (a, w) }
  deriving (Functor, Foldable, Traversable) -- these may be poorly optimized?

instance (Show w, Show a) => Show (Weighted w a) where
  show = show . asPair

instance (Show w) => Show1 (Weighted w) where
  showsPrec1 = showsPrec

-- we derive these for now

--instance Functor (Weighted w) where
  --{-# INLINABLE fmap #-}
  --fmap f = Weighted . (_1 %~ f) . asPair
--  fmap f (Weighted (a, w)) = Weighted (f a, w)

--instance Foldable (Weighted w) where
--  foldMap f (Weighted (a, _)) = f a

--instance Traversable (Weighted w) where
  --{-# INLINABLE sequenceA #-}
--  sequenceA (Weighted (as, w)) = fmap (Weighted . (, w)) as

-- the constraint really should be (Monoid w)
-- for conveniance we specialize the Product instance instead
instance (Ring.C w) => Monad' (Weighted w) where
  return' a = Weighted (a, one)
  --{-# INLINABLE join' #-}
  join' (Weighted (Weighted (a, w0), w1)) = Weighted (a, w0 * w1)

-- boilerplate conversion from Monad' to Monad
instance (Ring.C w) => Monad (Weighted w) where
  return = return'
  (>>=) = bind'

instance (Ring.C w) => Applicative (Weighted w) where
  pure = return'
  (<*>) = ap'

type Discrete w = Compose [] (Weighted w)
--type DiscreteT w m = Compose [] (WriterT w m)

toDiscrete :: [(a, w)] -> Discrete w a
toDiscrete = Compose . (map Weighted)

--{-# INLINABLE expectation #-}
expectation :: (Module.C w a) => Discrete w a -> a
expectation = (foldl' (+) zero) . fmap (\(Weighted (a, w)) -> w *> a) . getCompose

instance (Field.C w) => MonadDiscrete w (Discrete w) where
  sample = toDiscrete

type RoseTree w = Free (Discrete w)

-- generic to any MonadFree (Discrete w)
instance (Field.C w) => MonadDiscrete w (RoseTree w) where
  sample = liftF . toDiscrete

instance (Field.C w) => MonadDiscrete w (F (Discrete w)) where
  sample = liftF . toDiscrete

class (Monad m) => MonadBit m where
  bit :: m Bool

  default bit :: MonadFree Bin m => m Bool
  bit = liftF $ Bin False True

instance MonadBit BinTree
instance MonadBit (F Bin)

instance (Field.C w) => MonadBit (Discrete w) where
  bit = uniform [False, True]

ifBit :: (Functor m, MonadBit m) => a -> a -> m a
ifBit x y = (\b -> if b then x else y) <$> bit

test :: (Prelude.Fractional w, Field.C w, MonadDiscrete w m) => m Int
test = do
  x <- sample [(True, 0.5), (False, 0.5)]
  y <- if x then (return 1) else sample [(1, 0.5), (2, 0.5)]
  return (y + 1)

test2 :: (Functor m, MonadBit m) => m Int
test2 = do
  x <- bit
  y <- if x then (return 1) else ifBit 1 2
  return (y + 1)
