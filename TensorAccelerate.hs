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
  UndecidableInstances
  #-}

module TensorAccelerate where

import Data.Array.Accelerate as A
import Data.Singletons.Prelude hiding ((:.))
import Generics.SOP.Constraint as C
import GenericTensor

type family Dims2Shape (dims :: [k]) :: * where
  Dims2Shape '[] = Z
  Dims2Shape (n ': dims) = Dims2Shape dims :. Int

-- a bit of redundancy in these types/proofs, but this is the cleanest thing I could do
-- the point of all this is to cache Shape (Dims2Shape dims) proofs
-- we could of course reconstruct them from SingI dims as needed
-- but this saves us some pain in the subsequent ATensor instances
data SDims dims where
  DimsZ :: SDims '[]
  DimsS :: SDimsI dims => Sing n -> SDims dims -> SDims (n ': dims)

-- we need to use All SingI dims instead of just SingI dims to help the typechecker
-- the issue is that ghc doesn't work backwards from instance declarations
-- that is, given instance A x => B x and B x, ghc can't deduce A x, even if B x has no overlaps
-- in contrast, the All constraint former is a just a type family and ghc will happily expand it
class (C.All SingI dims, IntegralL dims, Shape (Dims2Shape dims)) => SDimsI dims where
  sDims :: SDims dims

instance (IntegralK ('KProxy :: KProxy k)) => SDimsI ('[] :: [k]) where
  sDims = DimsZ

instance (SingI n, SDimsI dims) => SDimsI (n ': dims) where
  sDims = DimsS sing sDims

data ATensor (dims :: [k]) a where
  ATensor :: (SDimsI dims, Elt a) => Acc (Array (Dims2Shape dims) a) -> ATensor dims a

instance Tensor ATensor where
  type N ATensor = C.And Elt IsNum
  
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

dims2Shape :: (IntegralL dims) => SDims dims -> Dims2Shape dims
dims2Shape DimsZ = Z
dims2Shape (DimsS n l) = dims2Shape l :. natVal' n

instance (SDimsI dims, IntegralL dims, Elt a, IsNum a) => Num (ATensor dims a) where
  ATensor a + ATensor b = ATensor $ A.zipWith (+) a b
  ATensor a - ATensor b = ATensor $ A.zipWith (-) a b
  ATensor a * ATensor b = ATensor $ A.zipWith (*) a b
  negate (ATensor a) = ATensor (A.map negate a)
  abs (ATensor a) = ATensor (A.map abs a)
  signum (ATensor a) = ATensor (A.map signum a)
  fromInteger n = ATensor $ fill (constant $ dims2Shape (sDims :: SDims dims)) (constant $ fromInteger n)

