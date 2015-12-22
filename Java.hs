{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Java where

import Data.Maybe (fromJust)

import Language.Java.Syntax
import Language.Java.Parser

import qualified GHC.Generics as GHC
import Generics.SOP
import Generics.SOP.NP
import Generics.SOP.TH

import Data.Constraint
import Data.Default

import GrammarNN
import JavaGeneric
import JavaGen
import TypeLevel
import Vec
import Nats
import Tensor
import Utils

data SList xs where
  SNil' :: SList '[]
  SCons' :: Sing x -> SList xs -> SList (x ': xs)

fromSing :: Sing xs -> SList xs
fromSing SNil = SNil'
fromSing SCons = SCons' sing (fromSing sing)

oneHot :: Num a => SNat n -> LT n -> Vec n a
oneHot (SS n) ZLT = VCons 1 $ Vec.replicate n 0
oneHot (SS n) (SLT m) = VCons 0 $ oneHot n m

numChars = snat (Proxy :: Proxy 95)
chars = fromJust $ Vec.fromList numChars [' ' .. '~']

instance Neural Char where
  type Size Char = ToNat 95

instance {-# OVERLAPPABLE #-} Neural t where
  type Size t = ToNat 95

initialParams :: (Default a) => FlipNP EncodeParams GenericTypes a
initialParams = FlipNP $ cpure_NP (Proxy::Proxy HasParams) def

newtype Params ts a = Params (NP (EncodeParams a) ts)

instance Encode GenericTypes Char where
  encode _ c = Primitive $ Repr $ fromVec $ oneHot numChars (fromJust $ Vec.lookup c chars)

instance Encode GenericTypes Int where
  encode _ i = Primitive $ Repr $ pure (fromIntegral i)

instance Encode GenericTypes Integer where
  encode _ i = Primitive $ Repr $ pure (fromIntegral i)

instance Encode GenericTypes Double where
  encode _ d = Primitive $ Repr $ pure (realToFrac d)

--encodeJava :: 


