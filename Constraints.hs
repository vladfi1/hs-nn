{-# LANGUAGE PolyKinds, ConstraintKinds, DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Constraints
  ( Forall (..)
  , ForallC (..)
  , Trivial
  , forallImplies
  , FlipC
  , CompC
  , ForallC1
  , module Data.Constraint
  ) where

import Data.Proxy
import Data.Constraint
--import Generics.SOP.Constraint
--import Generics.SOP.Sing
--import Data.Singletons.Prelude.List hiding (All)

newtype Forall c = Forall (forall a. Dict (c a))

class ForallC c where
  forallC :: Forall c

class Trivial a
instance Trivial a

instance ForallC Trivial where
  forallC = Forall Dict

forallImplies :: ForallC c :- c a
forallImplies = Sub $
  case forallC of
    Forall dict -> dict


-- AmbiguousTypes :(
--withForall :: forall c b r. (forall a. (Dict (c a))) -> (c b => r) -> r
--withForall (Dict :: Dict (c b)) r = r

--withForall :: forall c a r. Forall c -> (c a => r) -> r
--withForall (Forall (Dict :: Dict (c a))) r = r

class c b a => FlipC c a b
instance c b a => FlipC c a b

instance Class (c b a) (FlipC c a b) where
  cls = Sub Dict

instance c b a :=> FlipC c a b where
  ins = Sub Dict

class c (f a) => CompC c f a
instance c (f a) => CompC c f a

instance Class (c (f a)) (CompC c f a) where
  cls = Sub Dict

instance c (f a) :=> CompC c f a where
  ins = Sub Dict

type ForallC1 c f = ForallC (CompC c f)

{-
data CList c xs where
  CNil :: CList c '[]
  CCons :: Dict (c x) -> CList c xs -> CList c (x ': xs)

class All c xs => AllC c xs where
  clist :: CList c xs

instance AllC c '[] where
  clist = CNil

instance (c x, AllC c xs) => AllC c (x ': xs) where
  clist = CCons Dict clist

type All2C c = AllC (AllC c)
type All3C c = AllC (All2C c)

forallImpliesAll :: forall c xs. SList xs -> ForallC c :- AllC c xs
forallImpliesAll SNil = Sub Dict
forallImpliesAll SCons = Sub $
  case (forallC :: Forall c) of
    Forall (Dict :: Dict (c a)) -> Dict \\ (forallImpliesAll sing)
-}
