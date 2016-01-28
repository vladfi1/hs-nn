-- TODO: better naming conventions

{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module GrammarNNDAG where

import Control.Monad (foldM)
import Data.Function (fix)

import Generics.SOP hiding (SingI)
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Constraint
import Generics.SOP.Dict
import Data.Vinyl hiding (Dict)
import Data.Singletons.Prelude hiding (All, And)
import Data.Singletons.Prelude.List (Length)

import Utils
import List
import Grammar
import GenericTensor
import TensorDAG
import Gradients
import DAGIO
import VinylUtils
import Random
import qualified Constraints as C

import Data.Default
import DefaultM

import Numeric.LinearAlgebra (Numeric)
import qualified Data.Vector.Storable as Vector

type family Size (s :: k -> *) t :: k
type IntegralS (s :: k -> *) = IntegralK ('KProxy :: KProxy k)

class SingI (Size s t) => KnownSize s t
instance SingI (Size s t) => KnownSize s t

class All2 (KnownSize s) (Code t) => KnownSizes s t
instance All2 (KnownSize s) (Code t) => KnownSizes s t

class SingI (FromInteger (Length (Code t)) :: k) => KnownCode (s :: k -> *) t
instance SingI (FromInteger (Length (Code t)) :: k) => KnownCode (s :: k -> *) t

class HasNodes t where
  getNodes :: t -> [Some Node]

newtype Repr tensor n = Repr { runRepr :: Node (tensor '[n]) }

--instance (Num (tensor '[n])) => DefaultM IO (Repr tensor n) where
--  defM = Repr <$> defM

instance (Tensor tensor, IntegralN n) => DefaultM IO (Repr tensor n) where
  defM = case tensorFloat of (C.Dict :: C.Dict (Floating (tensor '[n]))) -> Repr <$> defM

instance HasNodes (Repr tensor n) where
  getNodes (Repr node) = [Some node]

newtype ReprT tensor s t = ReprT { runReprT :: Node (tensor '[Size s t]) }

instance (Tensor tensor, IntegralS s, KnownSize s t) => DefaultM IO (ReprT tensor s t) where
  defM = case tensorFloat of (C.Dict :: C.Dict (Floating (tensor '[Size s t]))) -> ReprT <$> defM

instance HasNodes (ReprT a s t) where
  getNodes (ReprT node) = [Some node]

--newtype Linear a outDim inDim = Linear (Node (tensor '[outDim, inDim] a))
newtype LinearT tensor s tOut tIn = LinearT (Node (tensor '[Size s tOut, Size s tIn]))

instance (Tensor tensor, IntegralL '[Size s tOut, Size s tIn]) => DefaultM IO (LinearT tensor s tOut tIn) where
  defM = case tensorFloat of (C.Dict :: C.Dict (Floating (tensor '[Size s tOut, Size s tIn]))) -> LinearT <$> defM

instance HasNodes (LinearT tensor s tOut tIn) where
  getNodes (LinearT node) = [Some node]

--linear :: Numeric a => Linear a outDim inDim -> Repr a inDim -> IO (Node (tensor a '[outDim]))
--linear (Linear m) (Repr v) = makeNode mvFun m v

linearT :: (Tensor tensor, IntegralS s, SingI (Size s tOut)) => LinearT tensor s tOut tIn -> ReprT tensor s tIn -> IO (ReprT tensor s tOut)
linearT (LinearT m) (ReprT v) = ReprT <$> makeMV m v

data EncodeParams' tensor s parent children = EncodeParams' (ReprT tensor s parent) (NP (LinearT tensor s parent) children)

instance HasNodes (EncodeParams' tensor s parent children) where
  getNodes (EncodeParams' bias weights) = getNodes bias ++ concat (collapse_NP $ liftA_NP' (K . getNodes) weights)

instance (Tensor tensor, IntegralS s, KnownSize s parent, SListI children, All (KnownSize s) children) => DefaultM IO (EncodeParams' tensor s parent children) where
  defM = EncodeParams' <$> defM <*> sequence'_NP (cpure_NP (Proxy::Proxy (KnownSize s)) (Comp defM))

encodeParent' :: forall tensor s parent children. (Tensor tensor, IntegralS s, KnownSize s parent) =>
  EncodeParams' tensor s parent children -> NP (ReprT tensor s) children -> IO (ReprT tensor s parent)
encodeParent' (EncodeParams' (ReprT bias) weights) inputs = do
  let linearT' m v = Comp $ K <$> linearT m v
  children <- map runReprT . collapse_NP <$> (sequence'_NP $ liftA2_NP' linearT' weights inputs)
  parent <- foldM (makeBinary (+)) bias children
  ReprT <$> makeUnary tanh parent

newtype EncodeParams tensor s t = EncodeParams { runEncodeParams :: NP (EncodeParams' tensor s t) (Code t) }

instance HasNodes (EncodeParams tensor s t) where
  getNodes (EncodeParams params) = concat . collapse_NP $ liftA_NP' (K . getNodes) params

encodeParent :: (Tensor tensor, IntegralS s, KnownSize s t) =>
  EncodeParams tensor s t -> SOP (ReprT tensor s) (Code t) -> IO (ReprT tensor s t)
encodeParent (EncodeParams params) (SOP sop) = collapse_NS $ liftA2_NS' encode params sop
  where encode p cs = K $ encodeParent' p cs

data Encoding tensor s t where
  Primitive :: ReprT tensor s t -> Encoding tensor s t
  Generic :: ReprT tensor s t -> SOP (Encoding tensor s) (Code t) -> Encoding tensor s t

getRepr :: Encoding tensor s t -> ReprT tensor s t
getRepr (Primitive repr) = repr
getRepr (Generic repr _) = repr

instance (Tensor tensor, IntegralS s, KnownSize s t, KnownSizes s t) => DefaultM IO (EncodeParams tensor s t) where
  defM = EncodeParams <$> sequence'_NP (cpure_NP (Proxy::Proxy (All (KnownSize s))) (Comp defM))

newtype Encoder tensor s t = Encoder { runEncoder :: t -> IO (Encoding tensor s t) }

encodeRec :: forall ts tensor s t. (Tensor tensor, KnownSize s t, IntegralS s) =>
  Rec (Encoder tensor s) ts -> Dict (Contained ts) t -> EncodeParams tensor s t -> Encoder tensor s t

encodeRec encoders Dict params = Encoder f where
  encoders' = cpure_POP (Proxy::Proxy (Find ts)) (Fn $ Comp . runEncoder (index find encoders) . unI)
  f t = do
    children <- sequence_SOP' (ap_SOP encoders' (from t))
    let childReprs = liftA_SOP' getRepr children
    parent <- encodeParent params childReprs
    return $ Generic parent children

makeEncoders :: forall p g tensor s. (Tensor tensor, IntegralS s, All (KnownSize s) g) =>
  Complete p g -> NP (EncodeParams tensor s) g -> Rec (Encoder tensor s) p -> Rec (Encoder tensor s) (p :++ g)
makeEncoders complete params prim = fix f where
  f encoders = rAppend prim $ np2rec (cliftA2_NP (Proxy::Proxy (KnownSize s)) (encodeRec encoders) (unAll_NP complete) params)

newtype AnyEncoder tensor s ts = AnyEncoder { runAnyEncoder :: forall t. Find ts t => t -> IO (Encoding tensor s t) }

makeEncoder :: (Tensor tensor, IntegralS s, All (KnownSize s) g) =>
  Complete p g -> NP (EncodeParams tensor s) g -> Rec (Encoder tensor s) p -> AnyEncoder tensor s (p :++ g)
makeEncoder complete params prim = AnyEncoder $ runEncoder (index find encoders)
  where encoders = makeEncoders complete params prim

newtype LinearIn tensor s t t' = LinearIn (Node (tensor '[Size s t', Size s t]))

instance HasNodes (LinearIn tensor s t t') where
  getNodes (LinearIn node) = [Some node]

instance (Tensor tensor, IntegralS s, KnownSize s t, KnownSize s t') => DefaultM IO (LinearIn tensor s t t') where
  defM = case tensorFloat of (C.Dict :: C.Dict (Floating (tensor '[Size s t', Size s t]))) -> LinearIn <$> defM

linearIn :: (IntegralS s, KnownSize s t', Tensor tensor) => LinearIn tensor s t t' -> ReprT tensor s t -> IO (ReprT tensor s t')
linearIn (LinearIn m) (ReprT v) = ReprT <$> makeMV m v

data AffineIn tensor s t t' = AffineIn (ReprT tensor s t') (LinearIn tensor s t t')

instance HasNodes (AffineIn tensor s t t') where
  getNodes (AffineIn bias linear) = getNodes bias ++ getNodes linear

instance (Tensor tensor, IntegralS s, KnownSize s t, KnownSize s t') => DefaultM IO (AffineIn tensor s t t') where
  defM = AffineIn <$> defM <*> defM

affineIn :: (Tensor tensor, IntegralS s, KnownSize s t') => AffineIn tensor s t t' -> ReprT tensor s t -> IO (ReprT tensor s t')
affineIn (AffineIn (ReprT bias) weights) input = do
  ReprT l <- linearIn weights input
  b <- makeBinary (+) bias l
  t <- makeUnary tanh b
  return $ ReprT t

data Affine' tensor outDim inDim =
  Affine' (Node (tensor '[outDim])) (Node (tensor '[outDim, inDim]))

instance HasNodes (Affine' tensor outDim inDim) where
  getNodes (Affine' bias linear) = [Some bias, Some linear]

instance (Tensor tensor, IntegralL '[outDim, inDim]) => DefaultM IO (Affine' tensor outDim inDim) where
  defM = case tensorFloat of
    (C.Dict :: C.Dict (Floating (tensor '[outDim, inDim]))) -> case tensorFloat of
      (C.Dict :: C.Dict (Floating (tensor '[outDim]))) -> Affine' <$> defM <*> defM

sigmoid x = 1 / (1 + exp (-x))

affine' :: (Tensor tensor, IntegralN outDim, SingI outDim) => Affine' tensor outDim inDim -> Repr tensor inDim -> IO (Repr tensor outDim)
affine' (Affine' bias weight) (Repr input) = do
  l <- makeMV weight input
  b <- makeBinary (+) bias l
  Repr <$> makeUnary sigmoid b

data DecodeParams tensor s t =
  DecodeParams (Affine' tensor (Length (Code t)) (Size s t)) (POP (AffineIn tensor s t) (Code t))

instance HasNodes (DecodeParams tensor s t) where
  getNodes (DecodeParams aff params) = getNodes aff ++ concat (collapse_POP' $ liftA_POP' (K . getNodes) params)

instance (Tensor tensor, KnownCode s t, KnownSize s t, KnownSizes s t) => DefaultM IO (DecodeParams tensor s t) where
  defM = DecodeParams <$> defM <*> sequence'_POP (cpure_POP (Proxy::Proxy (KnownSize s)) (Comp defM))

decodeParent :: forall tensor s t. (Tensor tensor, All2 (KnownSize s) (Code t), KnownCode s t) =>
  DecodeParams tensor s t -> ReprT tensor s t -> IO (SOP (ReprT tensor s) (Code t))
decodeParent (DecodeParams aff params) parent = do
  Repr node <- affine' aff (Repr $ runReprT parent)
  --Vector v <- evalNode node
  --let weights = map toRational $ Vector.toList v
  let weights = undefined -- FIXME: reimplement!
  
  let children = np2ns . unPOP $ cliftA_POP (Proxy::Proxy (KnownSize s)) (Comp . flip affineIn parent) params
  
  child <- sample $ zip children weights
  sequence'_SOP $ SOP child

newtype Decoder tensor s t = Decoder { runDecoder :: ReprT tensor s t -> IO t }

decodeRec :: forall ts tensor s t. (Tensor tensor, KnownSizes s t, KnownCode s t) =>
  Rec (Decoder tensor s) ts -> Dict (Contained ts) t -> DecodeParams tensor s t -> Decoder tensor s t

decodeRec decoders Dict params = Decoder f where
  decoders' = cpure_POP (Proxy::Proxy (Find ts)) (Fn $ Comp . (I <$>) . runDecoder (index find decoders))
  f repr = do
    childReprs <- decodeParent params repr
    children <- sequence_SOP' (ap_SOP decoders' childReprs)
    return $ to children

makeDecoders :: forall p g tensor s. (Tensor tensor, All (And (KnownSizes s) (KnownCode s)) g) =>
  Complete p g -> NP (DecodeParams tensor s) g -> Rec (Decoder tensor s) p -> Rec (Decoder tensor s) (p :++ g)
makeDecoders complete params prim = fix f where
  f decoders = rAppend prim $ np2rec (cliftA2_NP (Proxy::Proxy (And (KnownSizes s) (KnownCode s))) (decodeRec decoders) (unAll_NP complete) params)

newtype AnyDecoder tensor s ts = AnyDecoder { runAnyDecoder :: forall t. Find ts t => ReprT tensor s t -> IO t }

makeDecoder :: (Tensor tensor, All (And (KnownSizes s) (KnownCode s)) g) =>
  Complete p g -> NP (DecodeParams tensor s) g -> Rec (Decoder tensor s) p -> AnyDecoder tensor s (p :++ g)
makeDecoder complete params prim = AnyDecoder $ runDecoder (index find decoders)
  where decoders = makeDecoders complete params prim

newtype AutoDecoder tensor s t = AutoDecoder { runAutoDecoder :: Encoding tensor s t -> IO (Node (tensor '[])) }

autoDecodeRec :: forall ts tensor s t. (Tensor tensor, KnownCode s t, All2 (KnownSize s) (Code t)) =>
  Rec (AutoDecoder tensor s) ts -> Dict (Contained ts) t -> DecodeParams tensor s t -> AutoDecoder tensor s t

autoDecodeRec autoDecoders Dict (DecodeParams aff params) = AutoDecoder f where
  autoDecoders' = cpure_POP (Proxy::Proxy (Find ts)) (Fn $ Comp . (K <$>) . runAutoDecoder (index find autoDecoders))
  f :: Encoding tensor s t -> IO (Node (tensor '[]))
  f (Generic parent children) = do
    Repr node <- affine' aff (Repr $ runReprT parent)
    let i = ns2int (unSOP children)
    
    prob <- makeSelect i node
    log_score <- makeUnary (negate . log) prob
    
    let childReprs = liftA_SOP getRepr children
    let decodings = cliftA_POP (Proxy::Proxy (KnownSize s)) (Comp . flip affineIn parent) params
    
    let dist :: forall t'. KnownSize s t' => (IO :.: ReprT tensor s) t' -> ReprT tensor s t' -> (IO :.: K (Node (tensor '[]))) t'
        dist dIO (ReprT c) = Comp $ do
          ReprT d <- unComp dIO
          diff <- makeBinary (-) d c
          norm <- makeDot diff diff
          return $ K norm
    
    norms <- collapse_SOP <$> (sequence_SOP' $ cliftA2_SOP (Proxy::Proxy (KnownSize s)) dist decodings childReprs)
    
    childNodes <- collapse_SOP <$> sequence_SOP' (ap_SOP autoDecoders' children)
    
    foldM (makeBinary (+)) log_score (norms ++ childNodes)

primAutoDecoder :: forall tensor s t. (Tensor tensor, IntegralS s, KnownSize s t) => AutoDecoder tensor s t
primAutoDecoder = AutoDecoder f where
  f (Primitive _) = case tensorFloat of (C.Dict :: C.Dict (Floating (tensor '[]))) -> makeSource 0

makeAutoDecoders :: forall proxy p g tensor s. (Tensor tensor, All (And (KnownSizes s) (KnownCode s)) g) =>
  Complete p g -> NP (DecodeParams tensor s) g -> Rec (AutoDecoder tensor s) p -> Rec (AutoDecoder tensor s) (p :++ g)
makeAutoDecoders complete params prim = fix f where
  --prim = np2rec $ pure_NP primitiveAutoDecoder :: Rec _ p
  f autoDecoders = rAppend prim $ np2rec (cliftA2_NP (Proxy::Proxy (And (KnownSizes s) (KnownCode s))) (autoDecodeRec autoDecoders) (unAll_NP complete) params)

newtype AnyAutoDecoder tensor s ts = AnyAutoDecoder { runAnyAutoDecoder :: forall t. Find ts t => Encoding tensor s t -> IO (Node (tensor '[])) }

makeAutoDecoder :: (Tensor tensor, All (And (KnownSizes s) (KnownCode s)) g) =>
  Complete p g -> NP (DecodeParams tensor s) g -> Rec (AutoDecoder tensor s) p -> AnyAutoDecoder tensor s (p :++ g)
makeAutoDecoder complete params prim = AnyAutoDecoder $ runAutoDecoder (index find autoDecoders)
  where autoDecoders = makeAutoDecoders complete params prim

newtype AutoEncoder tensor s t = AutoEncoder { runAutoEncoder :: t -> IO (Node (tensor '[])) }

makeAutoEncoders :: forall p g tensor s. (Tensor tensor, All (KnownSize s) g, All (And (KnownSizes s) (KnownCode s)) g) =>
  Complete p g -> NP (EncodeParams tensor s) g -> Rec (Encoder tensor s) p -> NP (DecodeParams tensor s) g -> Rec (AutoDecoder tensor s) p -> Rec (AutoEncoder tensor s) (p :++ g)

makeAutoEncoders complete encodeParams primEncoders decodeParams primAutoDecoders = autoEncoders where
  encoders = makeEncoders complete encodeParams primEncoders
  autoDecoders = makeAutoDecoders complete decodeParams primAutoDecoders
  
  makeAutoEncoder (Encoder e) (AutoDecoder d) = AutoEncoder f where
    f t = e t >>= d
  
  autoEncoders = rZipWith makeAutoEncoder encoders autoDecoders

newtype AnyAutoEncoder tensor s ts = AnyAutoEncoder { runAnyAutoEncoder :: forall t. Find ts t => t -> IO (Node (tensor '[])) }

makeAutoEncoder :: (Tensor tensor, All (KnownSize s) g, All (And (KnownSizes s) (KnownCode s)) g) =>
  Complete p g -> NP (EncodeParams tensor s) g -> Rec (Encoder tensor s) p -> NP (DecodeParams tensor s) g -> Rec (AutoDecoder tensor s) p -> AnyAutoEncoder tensor s (p :++ g)
makeAutoEncoder complete encodeParams primEncoders decodeParams primAutoDecoders = AnyAutoEncoder $ runAutoEncoder (index find autoEncoders)
  where autoEncoders = makeAutoEncoders complete encodeParams primEncoders decodeParams primAutoDecoders

