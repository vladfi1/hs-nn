{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Vec where

import Prelude hiding (replicate, reverse, concat)
import Data.Singletons.Prelude

import Zippable
import Nats
import Data.Default
import Data.Foldable (toList)

data Vec n a where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

index :: LT n -> Vec n a -> a
index LT_Z (VCons a _) = a
index (LT_S n) (VCons _ l) = index n l

instance Functor (Vec n) where
  fmap f VNil = VNil
  fmap f (VCons a l) = VCons (f a) (fmap f l)

instance Foldable (Vec n) where
  foldr f b VNil = b
  foldr f b (VCons a l) = f a (foldr f b l)
  
  foldl f b VNil = b
  foldl f b (VCons a l) = foldl f (f b a) l

instance Traversable (Vec n) where
  sequenceA VNil = pure VNil
  sequenceA (VCons a l) = VCons <$> a <*> sequenceA l

replicate :: SNat n -> a -> Vec n a
replicate SZ _ = VNil
replicate (SS n) a = VCons a (replicate n a)

instance Zippable (Vec n) where
  VNil <**> VNil = VNil
  VCons f fs <**> VCons a as = VCons (f a) (fs <**> as)

-- ZipVec semantics
instance (SingI n) => Applicative (Vec n) where
  pure = replicate sing
  (<*>) = (<**>)

instance Show a => Show (Vec n a) where
  show = show . toList

instance (SingI n, Default a) => Default (Vec n a) where
  def = pure def

append :: Vec n a -> Vec m a -> Vec (n :+ m) a
append VNil v = v
append (VCons a v1) v2 = VCons a (append v1 v2)

fromList :: SNat n -> [a] -> Maybe (Vec n a)
fromList SZ [] = Just VNil
fromList (SS n) (a : as) = VCons a <$> fromList n as
fromList _ _ = Nothing

lookup :: Eq a => a -> Vec n a -> Maybe (LT n)
lookup _ VNil = Nothing
lookup a (VCons h t) =
  if a == h
    then Just LT_Z
    else LT_S <$> Vec.lookup a t

