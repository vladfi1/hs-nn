{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module GrammarNN where

import Generics.SOP
import Generics.SOP.NP
import Generics.SOP.NS
import qualified Generics.SOP as SOP
--import Data.Vinyl

import Constraints
import Utils
import List
import Tensor
import Nats
import TypeLevel
import Zippable

import Data.Default

import Prelude hiding (zipWith)

type family Codes xs where
  Codes '[] = '[]
  Codes (x ': xs) = Code x ': Codes xs

type family All3 (c :: k -> Constraint) (ksss :: [[[k]]]) :: Constraint where
  All3 c '[] = ()
  All3 c (kss ': ksss) = (All2 c kss, All3 c ksss)

--newtype FloatTensor n = FloatTensor { getFloatTensor :: Tensor n Float }

class Neural a where
  type Size a :: Nat

--foldrFlipNP :: Forall (CompC Foldable f) -> 

-- need to postpone applying the dimension
newtype Repr dim a = Repr { runRepr :: Tensor '[dim] a }
  deriving (Show, Functor, Foldable, Traversable)

instance ForallC (CompC Functor Repr) where
  forallC = Forall Dict

instance ForallC (CompC Foldable Repr) where
  forallC = Forall Dict

instance ForallC (CompC Traversable Repr) where
  forallC = Forall Dict

newtype Linear outDim inDim a = Linear (Tensor '[outDim, inDim] a)
  deriving (Show, Default, Functor, Foldable, Traversable)

instance ForallC (CompC Functor (Linear outDim)) where
  forallC = Forall Dict

instance ForallC (CompC Foldable (Linear outDim)) where
  forallC = Forall Dict

instance ForallC (CompC Traversable (Linear outDim)) where
  forallC = Forall Dict

linear :: Num a => Linear outDim inDim a -> Repr inDim a -> Tensor '[outDim] a
linear (Linear m) (Repr v) = mv m v

data Affine outDim inDims a = Affine (Tensor '[outDim] a) (FlipNP (Linear outDim) inDims a)
  deriving (Functor, Foldable, Traversable)

--deriving instance (AllC (CompC Show (Flip (Linear outDim) a)) inDims) => Show (Affine outDim inDims a)

instance ForallC (CompC Functor (Affine outDim)) where
  forallC = Forall Dict

instance ForallC (CompC Foldable (Affine outDim)) where
  forallC = Forall Dict

instance ForallC (CompC Traversable (Affine outDim)) where
  forallC = Forall Dict

instance (Default a, SingI inDims, SingI outDim, All SingI inDims) => Default (Affine outDim inDims a) where
  def = Affine def (FlipNP $ cpure_NP (Proxy::Proxy SingI) def)

affine :: Num a => Affine outDim inDims a -> FlipNP Repr inDims a -> Tensor '[outDim] a
affine (Affine bias weights) inputs =
  foldl (zipWith (+)) bias (map runRepr . collapse_FNP $ liftA2_FNP linear' weights inputs)
  where linear' m v = FK $ Repr $ linear m v

type family MapSize ts where
  MapSize '[] = '[]
  MapSize (t ': ts) = Size t ': MapSize ts

type family MapSize2 (tss :: [[*]]) :: [[Nat]] where
  MapSize2 '[] = '[]
  MapSize2 (ts ': tss) = MapSize ts ': MapSize2 tss

type family MapSum dimss where
  MapSum '[] = '[]
  MapSum (dims ': dimss) = Sum dims ': MapSum dimss

encodeParent :: (Floating a) =>
  FlipNP (Affine outDim) inDims a -> FlipSOP Repr inDims a -> Repr outDim a

encodeParent params (FlipSOP sop) = Repr $ tanh <$> (collapse_FNS $ liftA2_FNS affine' params sop)
  where affine' p i = FK $ affine p i

data Encoding t a where
  Primitive :: Neural t => Repr (Size t) a -> Encoding t a
  Generic :: (Neural t, Generic t) => Repr (Size t) a -> FlipSOP Encoding (Code t) a -> Encoding t a

instance Show a => Show (Encoding t a) where
  show (Primitive repr) = show repr
  show (Generic repr children) = showAsList . map unK . collapse_FSOP $ liftA_FSOP (FK . K . show) children

getRepr :: Encoding t a -> Repr (Size t) a
getRepr (Primitive repr) = repr
getRepr (Generic repr _) = repr

-- no way to write these with combinators?
getReprNP' :: NP (Flip Encoding a) ts -> NP (Flip Repr a) (MapSize ts)
getReprNP' Nil = Nil
getReprNP' (Flip e :* es) = Flip (getRepr e) :* getReprNP' es

getReprNP :: FlipNP Encoding ts a -> FlipNP Repr (MapSize ts) a
getReprNP (FlipNP np) = FlipNP $ getReprNP' np

getReprSOP' :: FlipNS (FlipNP Encoding) ts a -> FlipNS (FlipNP Repr) (MapSize2 ts) a
getReprSOP' (FlipNS (SOP.S es)) = FlipNS . SOP.S . runFlipNS $ getReprSOP' (FlipNS es)
getReprSOP' (FlipNS (SOP.Z (Flip es))) = FlipNS . SOP.Z . Flip $ getReprNP es

getReprSOP :: FlipSOP Encoding ts a -> FlipSOP Repr (MapSize2 ts) a
getReprSOP (FlipSOP sop) = FlipSOP $ getReprSOP' sop

newtype EncodeParams t a =
  EncodeParams { runEncodeParams :: FlipNP (Affine (Size t)) (MapSize2 (Code t)) a }
  deriving (Functor, Foldable, Traversable)

instance ForallC (CompC Foldable EncodeParams) where
  forallC = Forall Dict

--deriving instance (SingI (MapSize2 (Code t))) => Show (EncodeParams t a)

-- should be able to just use SingI here?
-- would have to write out the proof SingI dims -> All ReifyNat dims
class (SingI dims, All SingI dims) => Blah dims
instance (SingI dims, All SingI dims) => Blah dims

class (Neural t, Generic t, SingI (Size t), SingI (MapSize2 (Code t)), All Blah (MapSize2 (Code t))) => HasParams t
instance (Neural t, Generic t, SingI (Size t), SingI (MapSize2 (Code t)), All Blah (MapSize2 (Code t))) => HasParams t

instance (Default a, HasParams t) => Default (EncodeParams t a) where
  def = EncodeParams . FlipNP $ cpure_NP (Proxy::Proxy Blah) def

class Encode ts t where
  encode :: Floating a => FlipNP EncodeParams ts a -> t -> Encoding t a

instance {-# OVERLAPPABLE #-}
  (SingI (Code t), All2 (Encode ts) (Code t), Generic t, Neural t, Find ts t)
  => Encode ts t where
  encode :: forall a. Floating a => FlipNP EncodeParams ts a -> t -> Encoding t a
  encode params t = Generic repr children
    where
      {-
      cliftA_SOP' :: forall f g. (forall t'. Encode t' => f t' -> g t') ->
        SOP f (Code t) -> SOP g (Code t)
      -}
      
      encode' :: forall t'. (Encode ts t') => I t' -> Flip Encoding a t'
      encode' (I t') = Flip $ encode params t'

      cliftA_SOP' = cliftA_SOP (Proxy :: Proxy (Encode ts))
      
      children :: FlipSOP Encoding (Code t) a
      children = FlipSOP . FlipNS . (liftA_NS' $ Flip . FlipNP) . unSOP $ cliftA_SOP' encode' (from t)
      
      Flip (EncodeParams param) = index (find :: Index ts t) (np2Rec $ runFlipNP params)
      repr = encodeParent param (getReprSOP children)
      --childReprs = liftA_SOP getRepr 

--data DecodeParams a t = DecodeParams (Affine a (Len 


