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

-- this class isn't too useful, since it requires an Identity around
-- the function's return type in order to determine how far to uncurry
class Uncurry l r c | l r -> c, c -> l, c -> r where
  uncurry :: c -> HList l -> r

instance Uncurry '[] r (Identity r) where
  uncurry (Identity r) RNil = r

instance Uncurry l r c => Uncurry (a ': l) r (a -> c) where
  uncurry f (Identity a :& args) = uncurry (f a) args

{-
type family UncurryN (n :: Nat) (f :: *) :: (l :: [*]) where
  UncurryN Z r = '[r]
  UncurryN (S n) (a -> f) = a ': UncurryN n f

uncurryN :: SNat n -> f -> 
-}

class UncurryN n c l r | n c -> l, n c -> r, l -> n, l r -> c where
  uncurryN :: SNat n -> c -> HList l -> Identity r

instance UncurryN Z r '[] r where
  uncurryN SZ r RNil = Identity r

instance UncurryN n c l r => UncurryN (S n) (a -> c) (a ': l) r where
  uncurryN (SS n) f (Identity a :& args) = uncurryN n (f a) args


uncurry0 :: r -> HList '[] -> Identity r
--uncurry0 r RNil = Identity r
uncurry0 = uncurryN s0

uncurry1 :: (a -> r) -> HList '[a] -> Identity r
--uncurry1 f (Identity a :& l) = uncurry0 (f a) l
uncurry1 = uncurryN s1

uncurry2 :: (a -> b -> r) -> HList '[a, b] -> Identity r
--uncurry2 f (Identity a :& l) = uncurry1 (f a) l
uncurry2 = uncurryN s2

