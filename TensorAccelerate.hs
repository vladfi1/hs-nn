{-# LANGUAGE
  GADTs,
  TypeFamilies,
  PolyKinds,
  DataKinds,
  TypeOperators,
  FlexibleContexts,
  RankNTypes,
  InstanceSigs,
  ScopedTypeVariables
  #-}

module TensorAccelerate where

import Data.Array.Accelerate as A
import Data.Singletons
import Generics.SOP.Constraint (And)
import GenericTensor

type family Dims2Shape (dims :: [k]) :: * where
  Dims2Shape '[] = Z
  Dims2Shape (n ': dims) = Dims2Shape dims :. Int

data SDims dims where
  DimsZ :: SDims '[]
  DimsS :: SDimsI dims => SDims (a ': dims)

class (Shape (Dims2Shape dims)) => SDimsI dims where
  sDims :: SDims dims

instance SDimsI '[] where
  sDims = DimsZ

instance SDimsI dims => SDimsI (a ': dims) where
  sDims = DimsS

data ATensor (dims :: [k]) a where
  ATensor :: (SDimsI dims, Elt a) => Acc (Array (Dims2Shape dims) a) -> ATensor dims a
  
instance Tensor ATensor where
  type N ATensor = And Elt IsNum
  
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
  oneHot :: forall n a. (N ATensor a, SingI n, IntegralN n) => Int -> ATensor '[n] a
  oneHot i = ATensor $ generate (constant $ Z :. natVal' (sing :: Sing n)) (\j -> cond (constant i ==* unindex1 j) 1 0)

