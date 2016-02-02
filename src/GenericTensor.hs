{-# LANGUAGE
  DataKinds,
  KindSignatures,
  ConstraintKinds,
  TypeFamilies,
  TypeOperators,
  PolyKinds,
  FlexibleContexts,
  RankNTypes,
  ScopedTypeVariables,
  FlexibleInstances,
  UndecidableInstances
  #-}

-- minimal common interface between HMatrixndccelerate
module GenericTensor where

import GHC.Exts (Constraint)
import Gradients
import Misc.VarArgs
import Misc.Constraints
import Generics.SOP.Constraint as C

import Data.Singletons.Prelude 

-- FIXME: find better place for these
type IntegralK (p :: KProxy k) = (SingKind p, Integral (DemoteRep p))

class (IntegralK ('KProxy :: KProxy k), SingI n) => IntegralN (n :: k)
instance (IntegralK ('KProxy :: KProxy k), SingI n) => IntegralN (n :: k)

class (IntegralK ('KProxy :: KProxy k), SingI l, C.All SingI l) => IntegralL (l :: [k])
instance (IntegralK ('KProxy :: KProxy k), SingI l, C.All SingI l) => IntegralL (l :: [k])

natVal' :: (Num a, IntegralN n) => Sing n -> a
natVal' = fromIntegral . fromSing

{-
FIXME: These functions are not very general. It might be possible to
       provide more basic tensor ops and reimplement these methods in
       terms of the more basic operations.
FIXME: Floating is too hard of a constraint - we might want to use Ints!
       We use it here to make typechecking easier later - code that only
       assumes (Tensor t) and (IntegralL dims) can deduce Floating. It may
       also be better to provide this implication as a method rather than
       as a superclass constraint.
TODO: Randomness? People might want to do dropout, sample on the GPU, variational auto-encoding.
-}
class (ForallC (ImpliesC1 IntegralL (CompC Floating t)), Num (N t)) => Tensor (t :: [k] -> *) where
  -- the underlying numeric type
  type N t :: *
  
  -- TODO: extract the whole Tensor?
  scalar :: t '[] -> N t
  
  fill :: (IntegralL dims) => Sing dims -> N t -> t dims
  
  asCol :: t '[n] -> t '[n, FromInteger 1]
  asRow :: t '[n] -> t '[FromInteger 1, n]
  
  transpose :: t '[n, m] -> t '[m, n]
  
  scale :: t '[] -> t dims -> t dims
  
  dot :: t '[n] -> t '[n] -> t '[]
  
  outer :: t '[n] -> t '[m] -> t '[n, m]
  outer a b = mm (asCol a) (asRow b)
  
  mv :: t '[n, m] -> t '[m] -> t '[n]
  mm :: t '[n, m] -> t '[m, p] -> t '[n, p]
  
  select :: Int -> t '[n] -> t '[]
  
  -- implement in terms of fill?
  oneHot :: (IntegralN n) => Int -> t '[n]

fill' :: (IntegralL dims, Tensor t) => N t -> t dims
fill' = fill sing

gradDot :: (Tensor t) => GradFunc '[t '[n], t '[n]] (t '[])
gradDot = GradFunc (uncurry2 dot) (makeGrad2 $ \a b g -> (scale g b, scale g a))

gradMV :: (Tensor t) => GradFunc '[t '[n, m], t '[m]] (t '[n])
gradMV = GradFunc (uncurry2 mv) (makeGrad2 $ \m v g -> (outer g v, mv (transpose m) g))

gradMM :: (Tensor t) => GradFunc '[t '[n, m], t '[m, k]] (t '[n, k])
gradMM = GradFunc (uncurry2 mm) (makeGrad2 $ \a b g -> (mm g (transpose b), mm (transpose a) g))

gradSelect :: forall t n. (SingI n, IntegralN n, Tensor t) => Int -> GradFunc '[t '[n]] (t '[])
gradSelect i = GradFunc (uncurry1 $ select i) (makeGrad1 $ \_ g -> scale g (oneHot i :: t '[n]))

