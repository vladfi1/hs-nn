{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Tensor where

import Data.Default
import Data.Singletons.Prelude
import Data.Vinyl
import Nats
import Vec
import Zippable

data Tensor dim a where
  ZTensor :: a -> Tensor '[] a
  STensor :: Vec n (Tensor dim a) -> Tensor (n ': dim) a

instance Functor (Tensor dim) where
  fmap f (ZTensor a) = ZTensor (f a)
  fmap f (STensor v) = STensor (fmap (fmap f) v)

instance Foldable (Tensor dim) where
  foldr f b (ZTensor a) = f a b
  foldr f b (STensor v) = foldr (flip $ foldr f) b v
  
  foldl f b (ZTensor a) = f b a
  foldl f b (STensor v) = foldl (foldl f) b v

instance Traversable (Tensor dim) where
  sequenceA (ZTensor a) = ZTensor <$> a
  sequenceA (STensor v) = STensor <$> traverse sequenceA v

instance Zippable (Tensor dim) where
  ZTensor f <**> ZTensor a = ZTensor (f a)
  STensor fs <**> STensor as = STensor ((<**>) <$> fs <**> as)

fill :: Sing dim -> a -> Tensor dim a
fill SNil = ZTensor
fill (SCons d ds) = STensor . Vec.replicate d . fill ds

instance SingI dim => Applicative (Tensor dim) where
  pure = fill sing
  (<*>) = (<**>)

instance Show a => Show (Tensor dim a) where
  show (ZTensor a) = show a
  show (STensor v) = show v

instance (SingI dim, Default a) => Default (Tensor dim a) where
  def = pure def

fromVec :: Vec n a -> Tensor '[n] a
fromVec v = STensor $ fmap ZTensor v

toVec :: Tensor '[n] a -> Vec n a
toVec (STensor v) = fmap (\(ZTensor a) -> a) v

append :: Tensor (n ': dim) a -> Tensor (m ': dim) a -> Tensor (n :+ m ': dim) a
append (STensor n) (STensor m) = STensor $ Vec.append n m

dot a b = foldl (+) 0 $ (*) <$> a <**> b

mv :: Num a => Tensor (n ': dim) a -> Tensor dim a -> Tensor '[n] a
mv (STensor m) v = fromVec $ dot v <$> m

