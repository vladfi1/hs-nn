{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Nats where

import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Singletons.Prelude.Enum

$(singletons [d|
  data Nat = Z | S Nat
    deriving (Eq, Ord)

  instance Enum Nat where
    succ n = S n

    pred Z = error "pred Z"
    pred (S n) = n

    toEnum n = if n == 0 then Z else S $ toEnum (n-1)

    fromEnum Z = 0
    fromEnum (S n) = 1 + (fromEnum n)

  instance Num Nat where
    Z + x = x
    S x + y = S (x + y)

    x - Z = x
    Z - S _ = error "Z - S _"
    S x - S y = x - y

    Z * _ = Z
    S x * y = y + (x * y)

    fromInteger n = if n == 0 then Z else S (fromInteger (n-1))

    negate Z = Z
    negate (S _) = error "negate (S _)"
    
    abs n = n
    signum _ = 1

  |])

instance Real Nat where
  toRational = toRational . fromEnum

instance Integral Nat where
  quotRem a b = (fromInteger q, fromInteger r)
    where (q, r) = quotRem (toInteger a) (toInteger b)
  toInteger = toInteger . fromEnum

{-
type family Sum (xs :: [Nat]) :: Nat where
  Sum '[] = Z
  Sum (x ': xs) = x :+ (Sum xs)

infixl 7 :/:
type family (x :: Nat) :/: (y :: Nat) :: Nat where
  Z :/: y = Z -- avoid infinite loops
  x :/: y =
    If (x :<: y) Z
      (S ((x :-: y) :/: y))

infixl 7 :%:
type family (x :: Nat) :%: (y :: Nat) :: Nat where
  Z :%: y = Z -- avoid infinite loops
  x :%: y =
    If (x :<: y) x
      ((x :-: y) :%: y)

type family GCD (m :: Nat) (n :: Nat) :: Nat where
  GCD Z m = m
  GCD m n =
    If (m :<: n)
      (GCD (n :-: m) m)
      (GCD (m :-: n) n)

gcd' :: p1 m -> p2 n -> Proxy (GCD m n)
gcd' _ _ = Proxy
-}

instance Show Nat where
  show = show . fromEnum

instance Show (SNat n) where
  show = show . fromSing

s0 = SZ
s1 = SS s0
s2 = SS s1
s3 = SS s2

data LTE n where
  LTE_Z :: LTE n
  LTE_S :: LTE n -> LTE (S n)

data LT n where
  LT_Z :: LT (S n)
  LT_S :: LT n -> LT (S n)

lts :: SNat n -> [LT n]
lts SZ = []
lts (SS n) = LT_Z : map LT_S (lts n)

