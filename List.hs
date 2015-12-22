{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module List where

import Data.Vinyl
import Data.Type.Equality
import Nats
import Data.Singletons.Prelude
import Data.Singletons.TH

{-
data SList l where
  SNil' :: SList '[]
  SCons' :: Sing a -> SList l -> SList (a ': l)

slist :: forall l. SingI l => SList l
slist = case sing :: Sing l of
  SNil -> SNil'
  SCons -> SCons' sing slist
-}

--class Offset

-- an index n into l such that l[n] = a
data Index (l :: [k]) a where
  ZIndex :: Index (t ': l) t
  SIndex :: Index l t -> Index (a ': l) t

index :: Index l a -> Rec f l -> f a
index ZIndex (a :& _) = a
index (SIndex i) (_ :& l) = index i l

instance TestEquality (Index l) where
  testEquality ZIndex ZIndex = Just Refl
  testEquality (SIndex i) (SIndex j) = do
    Refl <- testEquality i j
    return Refl
  testEquality _ _ = Nothing

indices :: SList l -> Rec (Index l) l
indices SNil = RNil
indices (SCons _ l) = ZIndex :& rmap SIndex (indices l)

class Find l a where
  find :: Index l a

instance {-# OVERLAPS #-} Find (a ': l) a where
  find = ZIndex

instance Find l a => Find (b ': l) a where
  find = SIndex find

$(singletons [d|
  len :: [a] -> Nat
  len [] = Z
  len (x:xs) = S (len xs)
  |])

