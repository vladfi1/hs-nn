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
{-# LANGUAGE UndecidableInstances #-}

module TensorHMatrix where

import Nats
import Numeric.LinearAlgebra as H
import Data.Vector.Storable as V
import Data.Default
import Data.Singletons.Prelude
import Generics.SOP.Constraint as C
import Constraints
import GenericTensor

import Prelude hiding (zipWith)

-- the kitchen sink of constraints
type Usable a = (Element a, Num a, Numeric a, Num (Vector a), Container Vector a, Floating a, Floating (Vector a))

data HTensor a (dims :: [k]) where
  Scalar :: !a -> HTensor a '[]
  Vector :: Vector a -> HTensor a '[n]
  Matrix :: Matrix a -> HTensor a '[n, m]

deriving instance (Show a, Element a) => Show (HTensor a dims)

fill :: forall a dims. (IntegralL dims, Num a, Container Vector a) => a -> HTensor a dims
fill a = case (sing :: Sing dims) of
  SNil -> Scalar a
  SCons n SNil -> Vector $ konst a (natVal' n)
  SCons m (SCons n SNil) -> Matrix $ konst a (natVal' m, natVal' n)
  _ -> error "HTensors only go up to dimension 2."

instance (IntegralL dims, Default a, Num a, Container Vector a) => Default (HTensor a dims) where
  def = fill def

--type instance IndexOf (HTensor l) = Rec LT l
--type instance IndexOf (HTensor l) = Rec (Const Int) l
--type instance IndexOf (HTensor l) = Vec (Length l) Int

{- Fails backwards fundep :(
instance Indexable (HTensor (n ': l) a) (HTensor l a) where
  Matrix m ! i = Vector (m ! i)
  Vector v ! i = Scalar (v ! i)
-}

instance Usable a => Tensor (HTensor a :: [k] -> *) where
  type N (HTensor a) = a

  transpose (Matrix m) = Matrix (tr' m)
  
  scale (Scalar a) (Vector v) = Vector (H.scale a v)

  dot (Vector a) (Vector b) = Scalar (a <.> b)
  
  outer (Vector a) (Vector b) = Matrix (H.outer a b)

  mv (Matrix m) (Vector v) = Vector $ m #> v

  mm (Matrix m1) (Matrix m2) = Matrix $ m1 <> m2

  select i (Vector v) = Scalar (v V.! i)

  oneHot :: forall (n :: k). (SingI n, IntegralN n) => Int -> HTensor a '[n]
  oneHot i = Vector $ (konst 0 (natVal' (sing::Sing n))) // [(i, 1)]

-- pretty much all of these instances should be automatically derivable
-- the only issue is that fromInteger needs to use natVal'

instance (Usable a, IntegralL dims) => Num (HTensor a dims) where
  Scalar a + Scalar b = Scalar (a + b)
  Vector a + Vector b = Vector (a + b)
  Matrix a + Matrix b = Matrix (a + b)

  Scalar a - Scalar b = Scalar (a - b)
  Vector a - Vector b = Vector (a - b)
  Matrix a - Matrix b = Matrix (a - b)

  Scalar a * Scalar b = Scalar (a * b)
  Vector a * Vector b = Vector (a * b)
  Matrix a * Matrix b = Matrix (a * b)
  
  negate (Scalar a) = Scalar (negate a)
  negate (Vector a) = Vector (negate a)
  negate (Matrix a) = Matrix (negate a)

  abs (Scalar a) = Scalar (abs a)
  abs (Vector a) = Vector (abs a)
  abs (Matrix a) = Matrix (abs a)
  
  signum (Vector a) = Vector (signum a)
  signum (Scalar a) = Scalar (signum a)
  signum (Matrix a) = Matrix (signum a)
  
  
  fromInteger = fill . fromInteger

instance (IntegralL dims, Usable a) => Fractional (HTensor a dims) where
  Scalar a / Scalar b = Scalar (a / b)
  Vector a / Vector b = Vector (a / b)
  Matrix a / Matrix b = Matrix (a / b)
  
  recip (Scalar a) = Scalar (recip a)
  recip (Vector a) = Vector (recip a)
  recip (Matrix a) = Matrix (recip a)

  fromRational = fill . fromRational

instance (IntegralL dims, Usable a) => Floating (HTensor a dims) where
  pi = fill pi

  exp (Scalar a) = Scalar (exp a)
  exp (Vector a) = Vector (exp a)
  
  log (Scalar a) = Scalar (log a)
  log (Vector a) = Vector (log a)

  sqrt (Scalar a) = Scalar (sqrt a)
  sqrt (Vector a) = Vector (sqrt a)

  Scalar a ** Scalar b = Scalar (a ** b)
  Vector a ** Vector b = Vector (a ** b)

  logBase (Scalar a) (Scalar b) = Scalar (logBase a b)
  logBase (Vector a) (Vector b) = Vector (logBase a b)

  sin (Scalar a) = Scalar (sin a)
  sin (Vector a) = Vector (sin a)

  cos (Scalar a) = Scalar (cos a)
  cos (Vector a) = Vector (cos a)

  tan (Scalar a) = Scalar (tan a)
  tan (Vector a) = Vector (tan a)

  asin (Scalar a) = Scalar (asin a)
  asin (Vector a) = Vector (asin a)

  acos (Scalar a) = Scalar (acos a)
  acos (Vector a) = Vector (acos a)

  atan (Scalar a) = Scalar (atan a)
  atan (Vector a) = Vector (atan a)

  sinh (Scalar a) = Scalar (sinh a)
  sinh (Vector a) = Vector (sinh a)

  cosh (Scalar a) = Scalar (cosh a)
  cosh (Vector a) = Vector (cosh a)

  tanh (Scalar a) = Scalar (tanh a)
  tanh (Vector a) = Vector (tanh a)

  asinh (Scalar a) = Scalar (asinh a)
  asinh (Vector a) = Vector (asinh a)

  acosh (Scalar a) = Scalar (acosh a)
  acosh (Vector a) = Vector (acosh a)

  atanh (Scalar a) = Scalar (atanh a)
  atanh (Vector a) = Vector (atanh a)

instance Usable a => ImpliesC (IntegralL dims) (CompC Floating (HTensor a) dims) where
  impliesC = Sub Dict

instance Usable a => ForallC (ImpliesC1 IntegralL (CompC Floating (HTensor a))) where
  forallC = Forall Dict

tmap :: (Container Vector a, Num a, Element b) => (a -> b) -> HTensor a dims -> HTensor b dims
tmap f (Scalar a) = Scalar $ f a
tmap f (Vector v) = Vector $ cmap f v
tmap f (Matrix m) = Matrix $ cmap f m

tZipWith :: (Storable a, Storable b, Storable c) => (a -> b -> c) -> HTensor a '[n] -> HTensor b '[n] -> HTensor c '[n]
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

