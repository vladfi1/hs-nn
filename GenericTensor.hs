{-# LANGUAGE
  DataKinds,
  KindSignatures,
  ConstraintKinds,
  TypeFamilies,
  TypeOperators,
  PolyKinds,
  FlexibleContexts,
  RankNTypes,
  ScopedTypeVariables
  #-}

-- minimal common interface between HMatrixndccelerate
module GenericTensor where

import GHC.Exts (Constraint)
import Gradients
import VarArgs
import Constraints

import Data.Singletons.Prelude

-- FIXME: find better place for these
type IntegralK (p :: KProxy k) = (SingKind p, Integral (DemoteRep p))
type IntegralN (n :: k) = IntegralK ('KProxy :: KProxy k)
type IntegralL (l :: [k]) = IntegralK ('KProxy :: KProxy k)

natVal' :: (Num a, IntegralN n) => Sing n -> a
natVal' = fromIntegral . fromSing

-- FIXME: these functions are not very general :(
-- TODO: impose singleton constraints on dimensions? or let the constructors handle this?
class ForallC1 Num t => Tensor (t :: [k] -> *) where
  type N t :: *
  
  --fill :: (IntegralL dims, Sing dims) => 
  
  asCol :: t '[n] -> t '[n, FromInteger 1]
  asRow :: t '[n] -> t '[FromInteger 1, n]
  
  transpose :: t '[n, m] -> t '[m, n]
  
  scale :: t '[] -> t dims -> t dims
  
  dot :: t '[n] -> t '[n] -> t '[]
  
  outer :: t '[n] -> t '[m] -> t '[n, m]
  outer a b = mm (asCol a) (asRow b)
  
  mv :: t '[n, m] -> t '[m] -> t '[n]
  mm :: t '[n, m] -> t '[m, p] -> t '[n, p]
  
  --select :: N t => Int -> t (n ': dims) -> t dims
  select :: Int -> t '[n] -> t '[]
  
  oneHot :: (SingI n, IntegralN n) => Int -> t '[n]

gradDot :: (Tensor t) => GradFunc '[t '[n], t '[n]] (t '[])
gradDot = GradFunc (uncurry2 dot) (makeGrad2 $ \a b g -> (scale g b, scale g a))

gradMV :: (Tensor t) => GradFunc '[t '[n, m], t '[m]] (t '[n])
gradMV = GradFunc (uncurry2 mv) (makeGrad2 $ \m v g -> (outer g v, mv (transpose m) g))

gradMM :: (Tensor t) => GradFunc '[t '[n, m], t '[m, k]] (t '[n, k])
gradMM = GradFunc (uncurry2 mm) (makeGrad2 $ \a b g -> (mm g (transpose b), mm (transpose a) g))

gradSelect :: forall t n. (SingI n, IntegralN n, Tensor t) => Int -> GradFunc '[t '[n]] (t '[])
gradSelect i = GradFunc (uncurry1 $ select i) (makeGrad1 $ \_ g -> scale g (oneHot i :: t '[n]))

