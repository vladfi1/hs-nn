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
import qualified Data.Array.Accelerate.Interpreter as I

import Data.Singletons.Prelude hiding ((:.))
import GenericTensor
import Misc.Constraints
import Generics.SOP.Constraint as C
import Data.Proxy

type family Dims2Shape (dims :: [k]) :: * where
  Dims2Shape '[] = Z
  Dims2Shape (n ': dims) = Dims2Shape dims :. Int

data ATensor b a (dims :: [k]) where
  ATensor :: (sh ~ Dims2Shape dims, Shape sh) => Acc (Array sh a) -> ATensor b a dims

class Backend b where
  run :: Arrays a => Proxy b -> Acc a -> a
  run1 :: (Arrays a, Arrays b) => Proxy b -> (Acc a -> Acc b) -> a -> b

data Interpreter

instance Backend Interpreter where
  run _ = I.run
  run1 _ = I.run1

-- TODO: tmap?
instance (Elt a, IsFloating a, Backend b) => Tensor (ATensor b a :: [k] -> *) where
  type N (ATensor b a) = a
  
  -- provided in accelerate-2.0.0.0
  compute (ATensor a) = ATensor $ (id >-> id) a
  
  scalar (ATensor a) = indexArray (run (Proxy::Proxy b) a) Z
  
  -- TODO: implement
  --asCol :: N t a => t '[n] a -> t '[n, FromInteger 1] a
  --asRow :: N t a => t '[n] a -> t '[FromInteger 1, n] a
  
  transpose (ATensor a) = ATensor $ A.transpose a
  
  scale (ATensor a) (ATensor v) = ATensor $ A.map (the a *) v 
  
  dot (ATensor a) (ATensor b) = ATensor $ A.fold (+) 0 $ A.zipWith (*) a b
  
  outer (ATensor a) (ATensor b) = ATensor $ A.zipWith (*) as bs where
    n = A.size a
    m = A.size b
    
    as = A.replicate (lift $ Z :. All :. m) a
    bs = A.replicate (lift $ Z :. n :. All) b

  mv (ATensor m) (ATensor v) = ATensor $ A.fold (+) 0 $ A.zipWith (*) m vs
    where
      n = indexHead . indexTail $ shape m
      vs = A.replicate (lift $ Z :. n :. All) v

  -- TODO: implement
  --mm :: (N t a) => t '[n, m] a -> t '[m, p] a -> t '[n, p] a
  
  select i (ATensor a) = ATensor $ slice a (lift $ Any :. i)
  
  -- FIXME: constant has bad performance?
  --fill :: forall (dims :: [k]). (IntegralL dims) => a -> ATensor a dims
  fill sDims a = case proveShape sDims of Dict -> ATensor $ A.fill (constant $ dims2Shape sDims) (constant a)
  
  -- FIXME: constant has bad performance?
  oneHot :: forall (n :: k). (IntegralN n) => Int -> ATensor b a '[n]
  oneHot i = ATensor $ generate (constant $ Z :. natVal' (sing :: Sing n)) (\j -> cond (i' ==* unindex1 j) 1 0)
    where i' = constant i

proveShape :: SList dims -> Dict (Shape (Dims2Shape dims))
proveShape SNil = Dict
proveShape (SCons _ l) = case proveShape l of Dict -> Dict

-- we avoid the (SingI dims) constraint in order to recurse on the SList
dims2Shape :: forall (dims :: [k]). (IntegralK ('KProxy :: KProxy k), C.All SingI dims) => SList dims -> Dims2Shape dims
dims2Shape SNil = Z
dims2Shape (SCons n l) = dims2Shape l :. natVal' n

instance (IntegralL dims, Elt a, IsFloating a, Backend b) => Num (ATensor b a dims) where
  ATensor a + ATensor b = ATensor $ A.zipWith (+) a b
  ATensor a - ATensor b = ATensor $ A.zipWith (-) a b
  ATensor a * ATensor b = ATensor $ A.zipWith (*) a b
  negate (ATensor a) = ATensor (A.map negate a)
  abs (ATensor a) = ATensor (A.map abs a)
  signum (ATensor a) = ATensor (A.map signum a)
  fromInteger n = fill' (fromInteger n)

instance (IntegralL dims, Elt a, IsFloating a, Backend b) => Fractional (ATensor b a dims) where
  ATensor a / ATensor b = ATensor $ A.zipWith (/) a b
  recip (ATensor a) = ATensor $ A.map recip a
  fromRational r = fill' (fromRational r)

instance (IntegralL dims, Elt a, IsFloating a, Backend b) => Floating (ATensor b a dims) where
  pi = fill' pi
  
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

instance (Elt a, IsFloating a, Backend b) => ImpliesC (IntegralL dims) (CompC Floating (ATensor b a) dims) where
  impliesC = Sub Dict

instance (Elt a, IsFloating a, Backend b) => ForallC (ImpliesC1 IntegralL (CompC Floating (ATensor b a))) where
  forallC = Forall Dict

instance (Elt a) => Show (ATensor b a dims) where
  show (ATensor a) = show a

