{-# LANGUAGE
  DataKinds,
  KindSignatures,
  ConstraintKinds,
  TypeFamilies,
  TypeOperators,
  PolyKinds,
  FlexibleContexts
  #-}

-- minimal common interface between HMatrix and Accelerate
module GenericTensor where

import GHC.Exts (Constraint)
import Gradients
import VarArgs

import Data.Singletons.Prelude

--type IntegralK (p :: KProxy k) = (SingKind p, Integral (DemoteRep p))
--type IntegralN (n :: k) = IntegralK ('KProxy :: KProxy k)

-- FIXME: these functions are not very general :(
class Tensor (t :: [k] -> * -> *) where
  -- types storable in a Tensor
  -- might not be necessary?
  --type C t :: * -> Constraint
  
  -- numeric constraint
  type N t :: * -> Constraint
  
  asCol :: N t a => t '[n] a -> t '[n, FromInteger 1] a
  asRow :: N t a => t '[n] a -> t '[FromInteger 1, n] a
  
  transpose :: N t a => t '[n, m] a -> t '[m, n] a
  
  scale :: (N t a) => t '[] a -> t dims a -> t dims a
  
  dot :: (N t a) => t '[n] a -> t '[n] a -> t '[] a
  
  outer :: (N t a) => t '[n] a -> t '[m] a -> t '[n, m] a
  outer a b = mm (asCol a) (asRow b)
  
  mv :: (N t a) => t '[n, m] a -> t '[m] a -> t '[n] a
  mm :: (N t a) => t '[n, m] a -> t '[m, p] a -> t '[n, p] a
  
  select :: N t a => Int -> t (n ': dims) a -> t dims a

gradDot :: (Tensor t, N t a) => GradFunc '[t '[n] a, t '[n] a] (t '[] a)
gradDot = GradFunc (uncurry2 dot) (makeGrad2 $ \a b g -> (scale g b, scale g a))

gradMV :: (Tensor t, N t a) => GradFunc '[t '[n, m] a, t '[m] a] (t '[n] a)
gradMV = GradFunc (uncurry2 mv) (makeGrad2 $ \m v g -> (outer g v, mv (transpose m) g))

gradMM :: (Tensor t, N t a) => GradFunc '[t '[n, m] a, t '[m, k] a] (t '[n, k] a)
gradMM = GradFunc (uncurry2 mm) (makeGrad2 $ \a b g -> (mm g (transpose b), mm (transpose a) g))

