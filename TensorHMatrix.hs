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
{-# LANGUAGE InstanceSigs #-}

module TensorHMatrix where

import Nats
import Numeric.LinearAlgebra as H
import Data.Vector.Storable as V
import Data.Default
import Data.Singletons.Prelude hiding (And)
import Generics.SOP.Constraint (And)
import Data.Vinyl

import GenericTensor

import Prelude hiding (zipWith)

type Usable a = (Element a, Num a, Numeric a, Num (Vector a), Container Vector a)

-- TODO: make this a data family?
data HTensor (dims :: [k]) a where
  Scalar :: a -> HTensor '[] a
  Vector :: Vector a -> HTensor '[n] a
  Matrix :: Matrix a -> HTensor '[n, m] a

deriving instance (Show a, Element a) => Show (HTensor dims a)

instance (Default a) => Default (HTensor '[] a) where
  def = Scalar def

fill1 :: forall a n. (SingI n, IntegralN n, Usable a) => a -> HTensor '[n] a
fill1 a = Vector $ konst a (natVal' (sing::Sing n))

instance (SingI n, IntegralN n, Default a, Usable a) => Default (HTensor '[n] a) where
  def = fill1 def

instance (SingI n, IntegralN n, SingI m, Default a, Usable a) => Default (HTensor '[n, m] a) where
  def = Matrix $ konst def (natVal' (sing::Sing n), natVal' (sing::Sing m))

--type instance IndexOf (HTensor l) = Rec LT l
--type instance IndexOf (HTensor l) = Rec (Const Int) l
--type instance IndexOf (HTensor l) = Vec (Length l) Int

{- Fails backwards fundep :(
instance Indexable (HTensor (n ': l) a) (HTensor l a) where
  Matrix m ! i = Vector (m ! i)
  Vector v ! i = Scalar (v ! i)
-}

instance Tensor HTensor where
  --type C HTensor = And Element (Container Vector)
  
  type N HTensor = Numeric

  transpose (Matrix m) = Matrix (tr' m)
  
  scale (Scalar a) (Vector v) = Vector (H.scale a v)

  dot (Vector a) (Vector b) = Scalar (a <.> b)
  
  outer (Vector a) (Vector b) = Matrix (H.outer a b)

  mv (Matrix m) (Vector v) = Vector $ m #> v

  mm (Matrix m1) (Matrix m2) = Matrix $ m1 <> m2

  select i (Vector v) = Scalar (v V.! i)

  oneHot :: forall a n. (SingI n, IntegralN n, Numeric a) => Int -> HTensor '[n] a
  oneHot i = Vector $ (konst 0 (natVal' (sing::Sing n))) // [(i, 1)]

-- pretty much all of these instances should be automatically derivable
-- the only issue is that fromInteger needs to use natVal'

instance Num a => Num (HTensor '[] a) where
  Scalar a + Scalar b = Scalar (a + b)
  Scalar a - Scalar b = Scalar (a - b)
  Scalar a * Scalar b = Scalar (a * b)
  negate (Scalar a) = Scalar (negate a)
  abs (Scalar a) = Scalar (abs a)
  signum (Scalar a) = Scalar (signum a)
  fromInteger n = Scalar (fromInteger n)

instance (SingI n, IntegralN n, Numeric a, Num (Vector a)) => Num (HTensor '[n] a) where
  Vector a + Vector b = Vector (a + b)
  Vector a - Vector b = Vector (a - b)
  Vector a * Vector b = Vector (a * b)
  negate (Vector a) = Vector (negate a)
  abs (Vector a) = Vector (abs a)
  signum (Vector a) = Vector (signum a)
  fromInteger n = Vector $ konst (fromInteger n) (natVal' (sing::Sing n))

instance (SingI n, IntegralN n, SingI m, Usable a) => Num (HTensor '[n, m] a) where
  Matrix a + Matrix b = Matrix (a + b)
  Matrix a - Matrix b = Matrix (a - b)
  Matrix a * Matrix b = Matrix (a * b)
  negate (Matrix a) = Matrix (negate a)
  abs (Matrix a) = Matrix (abs a)
  signum (Matrix a) = Matrix (signum a)
  fromInteger n = Matrix $ konst (fromInteger n) (natVal' (sing::Sing n), natVal' (sing::Sing m))

instance Fractional a => Fractional (HTensor '[] a) where
  Scalar a / Scalar b = Scalar (a / b)
  recip (Scalar a) = Scalar (recip a)
  fromRational r = Scalar (fromRational r)

instance (SingI n, IntegralN n, Numeric a, Fractional a, Fractional (Vector a)) => Fractional (HTensor '[n] a) where
  Vector a / Vector b = Vector (a / b)
  recip (Vector a) = Vector (recip a)
  fromRational r = Vector $ konst (fromRational r) (natVal' (sing::Sing n))

instance Floating a => Floating (HTensor '[] a) where
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

instance (SingI n, IntegralN n, Usable a, Floating a, Floating (Vector a)) => Floating (HTensor '[n] a) where
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

tmap :: (Container Vector a, Num a, Element b) => (a -> b) -> HTensor dims a -> HTensor dims b
tmap f (Scalar a) = Scalar $ f a
tmap f (Vector v) = Vector $ cmap f v
tmap f (Matrix m) = Matrix $ cmap f m

tZipWith :: (Storable a, Storable b, Storable c) => (a -> b -> c) -> HTensor '[n] a -> HTensor '[n] b -> HTensor '[n] c
tZipWith f (Vector a) (Vector b) = Vector (zipWith f a b)

{-
transpose :: (Numeric a) => HTensor '[n, m] a -> HTensor '[m, n] a
transpose (Matrix m) = Matrix (tr' m)

asCol :: Storable a => HTensor '[n] a -> HTensor '[n, FromInteger 1] a
asCol (Vector v) = Matrix $ asColumn v

asRow' :: Storable a => HTensor '[n] a -> HTensor '[FromInteger 1, n] a
asRow' (Vector v) = Matrix $ asRow v

dot :: Numeric a => HTensor '[n] a -> HTensor '[n] a -> a
dot (Vector a) (Vector b) = a <.> b

mv :: Numeric a => HTensor '[n, m] a -> HTensor '[m] a -> HTensor '[n] a
mv (Matrix m) (Vector v) = Vector $ m #> v

mm :: Numeric a => HTensor '[n, m] a -> HTensor '[m, k] a -> HTensor '[n, k] a
mm (Matrix m1) (Matrix m2) = Matrix $ m1 <> m2

gradDot :: Numeric a => HTensor '[n] a -> HTensor '[n] a -> a -> (HTensor '[n] a, HTensor '[n] a)
gradDot (Vector a) (Vector b) g = (Vector $ scale g b, Vector $ scale g a)

gradMV :: Numeric a => HTensor '[n, m] a -> HTensor '[m] a -> HTensor '[n] a -> (HTensor '[n, m] a, HTensor '[m] a)
gradMV m v g = (mm (asCol g) (asRow' v), mv (transpose m) g)

gradMM :: Numeric a => HTensor '[n, m] a -> HTensor '[m, k] a -> HTensor '[n, k] a -> (HTensor '[n, m] a, HTensor '[m, k] a)
gradMM a b g = (mm g (transpose b), mm (transpose a) g)

gradSelect :: forall a n. (SingI n, IntegralN n, Usable a) => Int -> HTensor '[n] a -> a -> HTensor '[n] a
gradSelect i _ a = Vector $ (konst 0 (natVal' (sing::Sing n))) // [(i, a)]
-}

