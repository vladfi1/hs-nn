{-# LANGUAGE
  GADTs,
  TypeFamilies,
  PolyKinds,
  DataKinds,
  TypeOperators,
  FlexibleContexts,
  RankNTypes,
  InstanceSigs,
  ScopedTypeVariables,
  UndecidableInstances,
  FlexibleInstances,
  MultiParamTypeClasses
  #-}

module TensorAccelerate where

import Data.Array.Accelerate hiding (fill)
import qualified Data.Array.Accelerate as A
import Data.Singletons.Prelude hiding ((:.))
import GenericTensor
import Constraints
import Generics.SOP.Constraint as C

type family Dims2Shape (dims :: [k]) :: * where
  Dims2Shape '[] = Z
  Dims2Shape (n ': dims) = Dims2Shape dims :. Int

data ATensor a (dims :: [k]) where
  ATensor :: (sh ~ Dims2Shape dims, Shape sh) => Acc (Array sh a) -> ATensor a dims

instance (Elt a, IsFloating a) => Tensor (ATensor a :: [k] -> *) where
  type N (ATensor a) = a
  
  --asCol :: N t a => t '[n] a -> t '[n, FromInteger 1] a
  --asRow :: N t a => t '[n] a -> t '[FromInteger 1, n] a
  
  transpose (ATensor a) = ATensor (A.transpose a)
  
  scale (ATensor a) (ATensor v) = ATensor $ A.map (the a *) v 
  
  dot (ATensor a) (ATensor b) = ATensor $ A.fold (+) 0 $ A.zipWith (*) a b
  
  mv (ATensor m) (ATensor v) = ATensor $ A.fold (+) 0 $ A.zipWith (*) m vs
    where
      n = indexHead . indexTail $ shape m
      vs = A.replicate (lift $ Z :. n :. All) v

  {-
  mm :: (N t a) => t '[n, m] a -> t '[m, p] a -> t '[n, p] a
  -}
  
  {-
  select :: forall n dims a. (N ATensor a) => Int -> ATensor (n ': dims) a -> ATensor dims a
  select i (ATensor a) =
    case (sDims :: SDims (n ': dims)) of
      DimsS -> ATensor $ slice a (lift $ Any :. i)
  -}
  
  select i (ATensor a) = ATensor $ slice a (lift $ Any :. i)
  
  -- FIXME: constant has bad performance?
  oneHot :: forall (n :: k). (IntegralN n) => Int -> ATensor a '[n]
  oneHot i = ATensor $ generate (constant $ Z :. natVal' (sing :: Sing n)) (\j -> cond (constant i ==* unindex1 j) 1 0)

proveShape :: SList dims -> Dict (Shape (Dims2Shape dims))
proveShape SNil = Dict
proveShape (SCons _ l) = case proveShape l of Dict -> Dict

-- we avoid the (SingI dims) constraint in order to recurse on the SList
dims2Shape :: forall (dims :: [k]). (IntegralK ('KProxy :: KProxy k), C.All SingI dims) => SList dims -> Dims2Shape dims
dims2Shape SNil = Z
dims2Shape (SCons n l) = dims2Shape l :. natVal' n

fill :: forall a dims. (IntegralL dims, Elt a) => a -> ATensor a dims
fill a = case proveShape sDims of Dict -> ATensor $ A.fill (constant $ dims2Shape sDims) (constant a)
    where sDims = sing :: SList dims

instance (IntegralL dims, Elt a, IsNum a) => Num (ATensor a dims) where
  ATensor a + ATensor b = ATensor $ A.zipWith (+) a b
  ATensor a - ATensor b = ATensor $ A.zipWith (-) a b
  ATensor a * ATensor b = ATensor $ A.zipWith (*) a b
  negate (ATensor a) = ATensor (A.map negate a)
  abs (ATensor a) = ATensor (A.map abs a)
  signum (ATensor a) = ATensor (A.map signum a)
  fromInteger n = fill (fromInteger n)

instance (IntegralL dims, Elt a, IsFloating a) => Fractional (ATensor a dims) where
  ATensor a / ATensor b = ATensor $ A.zipWith (/) a b
  recip (ATensor a) = ATensor $ A.map recip a
  fromRational r = fill (fromRational r)

instance (IntegralL dims, Elt a, IsFloating a) => Floating (ATensor a dims) where
  pi = fill pi
  
  exp (ATensor a) = ATensor $ A.map exp a
  log (ATensor a) = ATensor $ A.map log a
  
  sin (ATensor a) = ATensor $ A.map sin a
  cos (ATensor a) = ATensor $ A.map cos a
  tan (ATensor a) = ATensor $ A.map tan a
  
  asin (ATensor a) = ATensor $ A.map asin a
  acos (ATensor a) = ATensor $ A.map acos a
  atan (ATensor a) = ATensor $ A.map atan a

  sinh (ATensor a) = ATensor $ A.map sinh a
  cosh (ATensor a) = ATensor $ A.map cosh a
  tanh (ATensor a) = ATensor $ A.map tanh a

  asinh (ATensor a) = ATensor $ A.map asinh a
  acosh (ATensor a) = ATensor $ A.map acosh a
  atanh (ATensor a) = ATensor $ A.map atanh a

instance (Elt a, IsFloating a) => ImpliesC (IntegralL dims) (CompC Floating (ATensor a) dims) where
  impliesC = Sub Dict

instance (Elt a, IsFloating a) => ForallC (ImpliesC1 IntegralL (CompC Floating (ATensor a))) where
  forallC = Forall Dict

