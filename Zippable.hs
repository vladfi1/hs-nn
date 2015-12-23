{-# LANGUAGE DefaultSignatures #-}

module Zippable where

import Prelude hiding (zipWith)

infixl 4 <**>

class Functor f => Zippable f where
  (<**>) :: f (a -> b) -> f a -> f b
  default (<**>) :: Applicative f => f (a -> b) -> f a -> f b
  (<**>) = (<*>)

instance Zippable []

zipWith :: Zippable f => (a -> b -> c) -> f a -> f b -> f c
zipWith f fa fb = f <$> fa <**> fb
  
zipWith3 :: Zippable f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
zipWith3 f fa fb fc = f <$> fa <**> fb <**> fc

