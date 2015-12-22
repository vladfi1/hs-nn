{-# LANGUAGE RankNTypes, GADTs, PolyKinds, TypeOperators #-}

module VinylUtils where

import Data.Singletons.Prelude
import Data.Vinyl

rtraverse_ :: Applicative h => (forall x. f x -> h (g x)) -> Rec f rs -> h ()
rtraverse_ f (x :& xs) = f x *> rtraverse_ f xs
rtraverse_ _ RNil = pure ()

rZipWith :: (forall x. f x -> g x -> h x) -> Rec f l -> Rec g l -> Rec h l
rZipWith _ RNil RNil = RNil
rZipWith f (fa :& fl) (ga :& gl) = f fa ga :& rZipWith f fl gl

rAppend :: Rec f l -> Rec f l' -> Rec f (l :++ l')
rAppend RNil l' = l'
rAppend (a :& l) l' = a :& rAppend l l'

liftA2_ :: Applicative h => (forall x. f x -> g x -> h ()) -> Rec f rs -> Rec g rs -> h ()
liftA2_ f (fx :& fxs) (gx :& gxs) = f fx gx *> liftA2_ f fxs gxs
liftA2_ _ RNil RNil = pure ()

rnull :: Rec f rs -> Bool
rnull RNil = True
rnull _ = False
