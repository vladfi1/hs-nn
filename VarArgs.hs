{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module VarArgs where

import Data.Vinyl
import Data.Vinyl.Functor
import Nats

import Prelude hiding (curry, uncurry)

{-
type family Curried f (l :: [k]) (r :: k) :: * where
  Curried f '[] r = r
  Curried f (a ': l) r = f a -> Curried f l r
-}

class Curry f l r c | f l r -> c where
  curry :: (Rec f l -> r) -> c

instance Curry f '[] r r where
  curry f = f RNil

instance Curry f l r c => Curry f (a ': l) r (f a -> c) where
  curry f a = curry (\args -> f $ a :& args)

uncurry0 :: r -> HList '[] -> Identity r
uncurry0 r RNil = Identity r

uncurry1 :: (a -> r) -> HList '[a] -> Identity r
uncurry1 f (Identity a :& l) = uncurry0 (f a) l

uncurry2 :: (a -> b -> r) -> HList '[a, b] -> Identity r
uncurry2 f (Identity a :& l) = uncurry1 (f a) l

-- use singletons instead of a custom class?
class Uncurry l r c | l r -> c, c -> l, c -> r where
  uncurry :: c -> HList l -> r

instance Uncurry '[] r (Identity r) where
  uncurry (Identity r) RNil = r

instance Uncurry l r c => Uncurry (a ': l) r (a -> c) where
  uncurry f (Identity a :& args) = uncurry (f a) args

{-
class Uncurry n f l r c | l -> n, n c -> l, n c -> r where
  uncurry :: SNat n -> c -> Rec f l -> r

instance Uncurry Z f '[] r r where
  uncurry SZ r RNil = r
-}

--instance Uncurry n f l r c => Uncurry (S n) f (a ': l) r (a -> c) where
--  uncurry (SS n) f (Identity a :& args) = uncurry n (f a) args

