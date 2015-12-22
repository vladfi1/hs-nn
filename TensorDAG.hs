{-# LANGUAGE GADTs, DataKinds, PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module TensorDAG where

import Data.Vinyl
import Data.Vinyl.Functor
import Data.Singletons
import Numeric.LinearAlgebra (Numeric)

import TensorHMatrix
import DAGIO
import VarArgs

makeGrad1 :: (a -> b -> a) -> HList '[a] -> Identity b -> HList '[a]
makeGrad1 g (Identity a :& RNil) (Identity b) = Identity (g a b) :& RNil

makeGrad2 :: (a -> b -> c -> (a, b)) -> HList '[a, b] -> Identity c -> HList '[a, b]
makeGrad2 g (Identity a :& Identity b :& RNil) (Identity c) = Identity a' :& Identity b' :& RNil
  where (a', b') = g a b c

makeDot :: Numeric a => Node (Tensor '[n] a) -> Node (Tensor '[n] a) -> IO (Node a)
makeDot = makeNode (uncurry2 dot, makeGrad2 gradDot)

makeMV :: (SingI n, IntegralN n, Usable a) => Node (Tensor '[n, m] a) -> Node (Tensor '[m] a) -> IO (Node (Tensor '[n] a))
makeMV = makeNode (uncurry2 mv, makeGrad2 gradMV)

makeMM :: (SingI n, IntegralN n, SingI k, Usable a) => Node (Tensor '[n, m] a) -> Node (Tensor '[m, k] a) -> IO (Node (Tensor '[n, k] a))
makeMM = makeNode (uncurry2 mm, makeGrad2 gradMM)

makeSelect i = makeNode (uncurry1 $ select i, makeGrad1 $ gradSelect i)

