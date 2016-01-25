{-# LANGUAGE GADTs, DataKinds, PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module TensorDAG where

import Data.Vinyl
import Data.Vinyl.Functor
--import Data.Singletons
--import Numeric.LinearAlgebra (Numeric)

import GenericTensor
import Gradients
import VarArgs


dotFun :: (Numeric a) => GradFunc '[Tensor '[n] a, Tensor '[n] a] a
dotFun = GradFunc (uncurry2 dot) (makeGrad2 gradDot)

makeDot :: (Numeric a) => Node (Tensor '[n] a) -> Node (Tensor '[n] a) -> IO (Node a)
makeDot = makeNode dotFun

mvFun :: Numeric a => GradFunc '[Tensor '[n, m] a, Tensor '[m] a] (Tensor '[n] a)
mvFun = GradFunc (uncurry2 mv) (makeGrad2 gradMV)

makeMV :: (SingI n, IntegralN n, Usable a) => Node (Tensor '[n, m] a) -> Node (Tensor '[m] a) -> IO (Node (Tensor '[n] a))
makeMV = makeNode mvFun

mmFun :: (SingI n, IntegralN n, Usable a) => GradFunc '[Tensor '[n, m] a, Tensor '[m, k] a] (Tensor '[n, k] a)
mmFun = GradFunc (uncurry2 mm) (makeGrad2 gradMM)
--makeMM = makeNode mmFun

--makeMM :: (SingI n, IntegralN n, SingI k, Usable a) => Node (Tensor '[n, m] a) -> Node (Tensor '[m, k] a) -> IO (Node (Tensor '[n, k] a))
--makeMM = makeNode (uncurry2 mm, makeGrad2 gradMM)

--makeSelect i = makeNode (uncurry1 $ select i, makeGrad1 $ gradSelect i)

selectFun i = GradFunc (uncurry1 $ select i) (makeGrad1 $ gradSelect i)
--makeSelect = makeNode . selectFun

