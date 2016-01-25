{-# LANGUAGE
  GADTs,
  TypeFamilies,
  PolyKinds,
  DataKinds,
  TypeOperators
  #-}

module TensorAccelerate where

import Data.Array.Accelerate as A
import Generics.SOP.Constraint
import GenericTensor

type family Dims2Shape (dims :: [k]) :: * where
  Dims2Shape '[] = Z
  Dims2Shape (n ': dims) = Dims2Shape dims :. Int

data ATensor (dims :: [k]) a where
  ATensor :: (sh ~ Dims2Shape dims, Shape sh, Elt a) => Acc (Array sh a) -> ATensor dims a

instance Tensor ATensor where
  type N ATensor = And Elt IsNum
  
  --asCol :: N t a => t '[n] a -> t '[n, FromInteger 1] a
  --asRow :: N t a => t '[n] a -> t '[FromInteger 1, n] a
  
  transpose (ATensor a) = ATensor (A.transpose a)
  
  scale (ATensor a) (ATensor v) = ATensor $ A.map (the a *) v 
  
  dot (ATensor a) (ATensor b) = ATensor $ A.fold (+) 0 $ A.zipWith (*) a b
  
  --mv :: (N t a) => t '[n, m] a -> t '[m] a -> t '[n] a
  mv (ATensor m) (ATensor v) = ATensor $ A.fold (+) 0 $ A.zipWith (*) m vs
    where
      n = indexHead . indexTail $ shape m
      vs = A.replicate (lift $ Z :. n :. All) v

  {-
  mm :: (N t a) => t '[n, m] a -> t '[m, p] a -> t '[n, p] a
  -}
  
  select :: N t a => Int -> t (n ': dims) a -> t dims a
  select i 
