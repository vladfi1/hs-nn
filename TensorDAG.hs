{-# LANGUAGE GADTs, DataKinds, PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TensorDAG where

--import Data.Vinyl
--import Data.Vinyl.Functor
--import Data.Singletons
--import Numeric.LinearAlgebra (Numeric)
import DAGIO
import GenericTensor
import Gradients
import VarArgs
import Constraints

tensorNum :: forall t a. Tensor t => Dict (Num (t a))
tensorNum =
  case (forallC :: Forall (CompC Num t)) of
    Forall (Dict :: Dict (CompC Num t a)) -> Dict

makeTensorNode :: forall t a. Tensor t => (Num (t a) => Node (t a)) -> Node (t a)
makeTensorNode n =
  case (forallC :: Forall (CompC Num t)) of
    Forall (Dict :: Dict (CompC Num t a)) -> n

makeDot :: forall n t. (Tensor t) => Node (t '[n]) -> Node (t '[n]) -> IO (Node (t '[]))
makeDot =
  case (forallC :: Forall (CompC Num t)) of
    Forall (Dict :: Dict (CompC Num t '[])) -> makeNode gradDot

--mvFun :: Tensor t => GradFunc '[t '[n, m] a, t '[m] a] (t '[n] a)
--mvFun = GradFunc (uncurry2 mv) (makeGrad2 gradMV)

makeMV :: forall t n m. (Tensor t) => Node (t '[n, m]) -> Node (t '[m]) -> IO (Node (t '[n]))
makeMV =
  case (forallC :: Forall (CompC Num t)) of
    Forall (Dict :: Dict (CompC Num t '[n])) -> makeNode gradMV

--makeMM = makeNode mmFun

--makeMM :: (SingI n, IntegralN n, SingI k, Usable a) => Node (Tensor '[n, m] a) -> Node (Tensor '[m, k] a) -> IO (Node (Tensor '[n, k] a))
--makeMM = makeNode (uncurry2 mm, makeGrad2 gradMM)

makeSelect i = makeNode (gradSelect i)

--selectFun i = GradFunc (uncurry1 $ select i) (makeGrad1 $ gradSelect i)
--makeSelect = makeNode . selectFun

makeUnary :: (Floating a) => (forall b. Floating b => b -> b) -> Node a -> IO (Node a)
makeUnary f = makeNode (unaryGrad f)

makeBinary :: (Num a) => (forall b. Num b => b -> b -> b) -> Node a -> Node a -> IO (Node a)
makeBinary f = makeNode (binaryGrad f)

