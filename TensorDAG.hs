{-# LANGUAGE GADTs, DataKinds, PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module TensorDAG where

--import Data.Vinyl
--import Data.Vinyl.Functor
--import Data.Singletons
--import Numeric.LinearAlgebra (Numeric)
import DAGIO
import GenericTensor
import Gradients
import VarArgs
import Data.Constraint
import Constraints

tensorFloat :: forall t dims. (Tensor t, IntegralL dims) => Dict (Floating (t dims))
tensorFloat =
  case (forallC :: Forall (ImpliesC1 IntegralL (CompC Floating t))) of
    Forall (Dict :: Dict (ImpliesC1 IntegralL (CompC Floating t) dims)) ->
      case (impliesC :: IntegralL dims :- (CompC Floating t dims)) of Sub Dict -> Dict

makeDot :: forall n t. (Tensor t, IntegralN n) => Node (t '[n]) -> Node (t '[n]) -> IO (Node (t '[]))
makeDot = case tensorFloat of (Dict :: Dict (Floating (t '[]))) -> makeNode gradDot

--mvFun :: Tensor t => GradFunc '[t '[n, m] a, t '[m] a] (t '[n] a)
--mvFun = GradFunc (uncurry2 mv) (makeGrad2 gradMV)

makeMV :: forall t n m. (Tensor t, IntegralN n) => Node (t '[n, m]) -> Node (t '[m]) -> IO (Node (t '[n]))
makeMV = case tensorFloat of (Dict :: Dict (Floating (t '[n]))) -> makeNode gradMV

--makeMM = makeNode mmFun

--makeMM :: (SingI n, IntegralN n, SingI k, Usable a) => Node (Tensor '[n, m] a) -> Node (Tensor '[m, k] a) -> IO (Node (Tensor '[n, k] a))
--makeMM = makeNode (uncurry2 mm, makeGrad2 gradMM)

makeSelect :: forall t n. (IntegralN n, Tensor t) => Int -> Node (t '[n]) -> IO (Node (t '[]))
makeSelect i = case tensorFloat of (Dict :: Dict (Floating (t '[]))) -> makeNode (gradSelect i)

makeUnary :: forall t dims. (Tensor t, IntegralL dims) => (forall b. Floating b => b -> b) -> Node (t dims) -> IO (Node (t dims))
makeUnary f = case tensorFloat of (Dict :: Dict (Floating (t dims))) -> makeNode (unaryGrad f)

makeBinary :: forall t dims. (Tensor t, IntegralL dims) => (forall b. Num b => b -> b -> b) -> Node (t dims) -> Node (t dims) -> IO (Node (t dims))
makeBinary f = case tensorFloat of (Dict :: Dict (Floating (t dims))) -> makeNode (binaryGrad f)

