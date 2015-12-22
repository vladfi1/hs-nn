{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TensorHMatrix where

import Nats
import Numeric.LinearAlgebra hiding ((!))
import Data.Vector.Storable
import Data.Default
import Data.Singletons.Prelude

import Data.Vinyl

import Prelude hiding (zipWith)

type Usable a = (Element a, Num a, Numeric a, Num (Vector a), Container Vector a)

type IntegralK (p :: KProxy k) = (SingKind p, Integral (DemoteRep p))
type IntegralN (n :: k) = IntegralK ('KProxy :: KProxy k)

natVal' :: (Num a, IntegralN n) => Sing n -> a
natVal' = fromIntegral . fromSing

data Tensor (dims :: [k]) a where
  Scalar :: a -> Tensor '[] a
  Vector :: Vector a -> Tensor '[n] a
  Matrix :: Matrix a -> Tensor '[n, m] a

deriving instance (Show a, Element a) => Show (Tensor dims a)

instance (Default a) => Default (Tensor '[] a) where
  def = Scalar def

fill1 :: forall a n. (SingI n, IntegralN n, Usable a) => a -> Tensor '[n] a
fill1 a = Vector $ konst a (natVal' (sing::Sing n))

instance (SingI n, IntegralN n, Default a, Usable a) => Default (Tensor '[n] a) where
  def = fill1 def

instance (SingI n, IntegralN n, SingI m, Default a, Usable a) => Default (Tensor '[n, m] a) where
  def = Matrix $ konst def (natVal' (sing::Sing n), natVal' (sing::Sing m))

--type instance IndexOf (Tensor l) = Rec LT l
--type instance IndexOf (Tensor l) = Rec (Const Int) l
--type instance IndexOf (Tensor l) = Vec (Length l) Int

{- Fails backwards fundep :(
instance Indexable (Tensor (n ': l) a) (Tensor l a) where
  Matrix m ! i = Vector (m ! i)
  Vector v ! i = Scalar (v ! i)
-}

select :: Storable a => Int -> Tensor '[n] a -> a
select i (Vector v) = v ! i

-- pretty much all of these instances should be automatically derivable

instance Num a => Num (Tensor '[] a) where
  Scalar a + Scalar b = Scalar (a + b)
  Scalar a - Scalar b = Scalar (a - b)
  Scalar a * Scalar b = Scalar (a * b)
  negate (Scalar a) = Scalar (negate a)
  abs (Scalar a) = Scalar (abs a)
  signum (Scalar a) = Scalar (signum a)
  fromInteger n = Scalar (fromInteger n)

instance (SingI n, IntegralN n, Usable a) => Num (Tensor '[n] a) where
  Vector a + Vector b = Vector (a + b)
  Vector a - Vector b = Vector (a - b)
  Vector a * Vector b = Vector (a * b)
  negate (Vector a) = Vector (negate a)
  abs (Vector a) = Vector (abs a)
  signum (Vector a) = Vector (signum a)
  fromInteger n = Vector $ konst (fromInteger n) (natVal' (sing::Sing n))

instance (SingI n, IntegralN n, SingI m, Usable a) => Num (Tensor '[n, m] a) where
  Matrix a + Matrix b = Matrix (a + b)
  Matrix a - Matrix b = Matrix (a - b)
  Matrix a * Matrix b = Matrix (a * b)
  negate (Matrix a) = Matrix (negate a)
  abs (Matrix a) = Matrix (abs a)
  signum (Matrix a) = Matrix (signum a)
  fromInteger n = Matrix $ konst (fromInteger n) (natVal' (sing::Sing n), natVal' (sing::Sing m))

instance Fractional a => Fractional (Tensor '[] a) where
  Scalar a / Scalar b = Scalar (a / b)
  recip (Scalar a) = Scalar (recip a)
  fromRational r = Scalar (fromRational r)

instance (SingI n, IntegralN n, Numeric a, Fractional a, Fractional (Vector a)) => Fractional (Tensor '[n] a) where
  Vector a / Vector b = Vector (a / b)
  recip (Vector a) = Vector (recip a)
  fromRational r = Vector $ konst (fromRational r) (natVal' (sing::Sing n))

instance Floating a => Floating (Tensor '[] a) where
  pi = Scalar pi
  exp (Scalar a) = Scalar (exp a)
  log (Scalar a) = Scalar (log a)
  sqrt (Scalar a) = Scalar (sqrt a)
  Scalar a ** Scalar b = Scalar (a ** b)
  logBase (Scalar a) (Scalar b) = Scalar (logBase a b)
  sin (Scalar a) = Scalar (sin a)
  cos (Scalar a) = Scalar (cos a)
  tan (Scalar a) = Scalar (tan a)
  asin (Scalar a) = Scalar (asin a)
  acos (Scalar a) = Scalar (acos a)
  atan (Scalar a) = Scalar (atan a)
  sinh (Scalar a) = Scalar (sinh a)
  cosh (Scalar a) = Scalar (cosh a)
  tanh (Scalar a) = Scalar (tanh a)
  asinh (Scalar a) = Scalar (asinh a)
  acosh (Scalar a) = Scalar (acosh a)
  atanh (Scalar a) = Scalar (atanh a)

instance (SingI n, IntegralN n, Usable a, Floating a, Floating (Vector a)) => Floating (Tensor '[n] a) where
  pi = fill1 pi
  exp (Vector a) = Vector (exp a)
  log (Vector a) = Vector (log a)
  sqrt (Vector a) = Vector (sqrt a)
  Vector a ** Vector b = Vector (a ** b)
  logBase (Vector a) (Vector b) = Vector (logBase a b)
  sin (Vector a) = Vector (sin a)
  cos (Vector a) = Vector (cos a)
  tan (Vector a) = Vector (tan a)
  asin (Vector a) = Vector (asin a)
  acos (Vector a) = Vector (acos a)
  atan (Vector a) = Vector (atan a)
  sinh (Vector a) = Vector (sinh a)
  cosh (Vector a) = Vector (cosh a)
  tanh (Vector a) = Vector (tanh a)
  asinh (Vector a) = Vector (asinh a)
  acosh (Vector a) = Vector (acosh a)
  atanh (Vector a) = Vector (atanh a)

tmap :: (Container Vector a, Num a, Element b) => (a -> b) -> Tensor dims a -> Tensor dims b
tmap f (Scalar a) = Scalar $ f a
tmap f (Vector v) = Vector $ cmap f v
tmap f (Matrix m) = Matrix $ cmap f m

tZipWith :: (Storable a, Storable b, Storable c) => (a -> b -> c) -> Tensor '[n] a -> Tensor '[n] b -> Tensor '[n] c
tZipWith f (Vector a) (Vector b) = Vector (zipWith f a b)

transpose :: (Numeric a) => Tensor '[n, m] a -> Tensor '[m, n] a
transpose (Matrix m) = Matrix (tr' m)

asCol :: Storable a => Tensor '[n] a -> Tensor '[n, FromInteger 1] a
asCol (Vector v) = Matrix $ asColumn v

asRow' :: Storable a => Tensor '[n] a -> Tensor '[FromInteger 1, n] a
asRow' (Vector v) = Matrix $ asRow v

dot :: (Storable a, Numeric a) => Tensor '[n] a -> Tensor '[n] a -> a
dot (Vector a) (Vector b) = a <.> b

mv :: Numeric a => Tensor '[n, m] a -> Tensor '[m] a -> Tensor '[n] a
mv (Matrix m) (Vector v) = Vector $ m #> v

mm :: Numeric a => Tensor '[n, m] a -> Tensor '[m, k] a -> Tensor '[n, k] a
mm (Matrix m1) (Matrix m2) = Matrix $ m1 <> m2

gradDot :: Numeric a => Tensor '[n] a -> Tensor '[n] a -> a -> (Tensor '[n] a, Tensor '[n] a)
gradDot (Vector a) (Vector b) g = (Vector $ scale g b, Vector $ scale g a)

gradMV :: Numeric a => Tensor '[n, m] a -> Tensor '[m] a -> Tensor '[n] a -> (Tensor '[n, m] a, Tensor '[m] a)
gradMV m v g = (mm (asCol g) (asRow' v), mv (transpose m) g)

gradMM :: Numeric a => Tensor '[n, m] a -> Tensor '[m, k] a -> Tensor '[n, k] a -> (Tensor '[n, m] a, Tensor '[m, k] a)
gradMM a b g = (mm g (transpose b), mm (transpose a) g)

oneHot :: forall a n. (SingI n, IntegralN n, Usable a) => Int -> Tensor '[n] a
oneHot m = Vector $ (konst 0 (natVal' (sing::Sing n))) // [(m, 1)]

gradSelect :: forall a n. (SingI n, IntegralN n, Usable a) => Int -> Tensor '[n] a -> a -> Tensor '[n] a
gradSelect i _ a = Vector $ (konst 0 (natVal' (sing::Sing n))) // [(i, a)]

