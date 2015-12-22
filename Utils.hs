{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Utils where

import Data.Vinyl hiding (Dict (..))
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.BasicFunctors
import Generics.SOP.Sing
import Generics.SOP.Constraint

import Constraints

import Data.Proxy
import Data.Default

--import Constraints

import Data.List (intercalate)

showAsList :: [String] -> String
showAsList strs = "[" ++ intercalate ", " strs ++ "]"

-- utilites for conversion between Vinyl and SOP
rec2np :: Rec f xs -> NP f xs
rec2np RNil = Nil
rec2np (fx :& fxs) = fx :* rec2np fxs

np2rec :: NP f xs -> Rec f xs
np2rec Nil = RNil
np2rec (fx :* fxs) = fx :& np2rec fxs

-- like liftA_NP but without the constraints
liftA_NP' :: (forall a. f a -> g a) -> NP f xs -> NP g xs
liftA_NP' _ Nil = Nil
liftA_NP' f (fx :* fxs) = f fx :* liftA_NP' f fxs

-- like liftA2_NS but without the constrains
liftA_NS' :: (forall a. f a -> g a) -> NS f xs -> NS g xs
liftA_NS' f (Z fx) = Z (f fx)
liftA_NS' f (S fxs) = S (liftA_NS' f fxs)

-- like liftA_SOP but without the constraints
liftA_SOP' :: (forall a. f a -> g a) -> SOP f xss -> SOP g xss
liftA_SOP' f (SOP sop) = SOP (liftA_NS' (liftA_NP' f) sop)

-- like liftA_POP but without the constraints
liftA_POP' :: (forall a. f a -> g a) -> POP f xss -> POP g xss
liftA_POP' f (POP pop) = POP (liftA_NP' (liftA_NP' f) pop)

-- like liftA2_NP but without the constraints
liftA2_NP' :: (forall a. f a -> g a -> h a) -> NP f xs -> NP g xs -> NP h xs
liftA2_NP' _ Nil Nil = Nil
liftA2_NP' f (x :* xs) (y :* ys) = f x y :* liftA2_NP' f xs ys

-- like liftA2_NS but without the constraint
liftA2_NS' :: forall f g h xs. (forall a. f a -> g a -> h a) -> NP f xs -> NS g xs -> NS h xs
liftA2_NS' f (fx :* _) (Z gx) = Z (f fx gx)
liftA2_NS' f (_ :* fxs) (S gxs) = S (liftA2_NS' f fxs gxs)

liftA2_SOP'' :: forall f g h xs. (forall a. f a -> g a -> h a) -> NP (NP f) xs -> NS (NP g) xs -> NS (NP h) xs
liftA2_SOP'' f (fxs :* _) (Z gxs) = Z (liftA2_NP' f fxs gxs)
liftA2_SOP'' f (_ :* fxss) (S gxss) = S (liftA2_SOP'' f fxss gxss)

-- like liftA2_SOP but without the constraints
liftA2_SOP' :: forall f g h xss. (forall a. f a -> g a -> h a) -> POP f xss -> SOP g xss -> SOP h xss
liftA2_SOP' f (POP pop) (SOP sop) = SOP $ liftA2_SOP'' f pop sop

collapse_SOP' :: SOP (K a) xs -> [a]
collapse_SOP' (SOP sop) = collapse_NS $ liftA_NS' (K . collapse_NP) sop

collapse_POP' :: POP (K a) xs -> [a]
collapse_POP' (POP pop) = concat . collapse_NP $ liftA_NP' (K . collapse_NP) pop

sequence_SOP' :: Applicative f => SOP (f :.: g) xss -> f (SOP g xss)
sequence_SOP' (SOP (Z fgxs)) = SOP . Z <$> sequence'_NP fgxs
sequence_SOP' (SOP (S ns)) = SOP . S . unSOP <$> sequence_SOP' (SOP ns)

np2ns :: NP f xs -> [NS f xs]
np2ns Nil = []
np2ns (fx :* fxs) = Z fx : map S (np2ns fxs)

ns2int :: NS f xs -> Int
ns2int (Z _) = 0
ns2int (S xs) = succ (ns2int xs)

newtype FK f b a = FK (f a)

newtype Flip f a b = Flip { runFlip :: f b a }
  deriving (Default)

instance Show (f b a) => Show (Flip f a b) where
  show = show . runFlip

showFlip :: forall f a b. ForallC1 Show (Flip f a) => Flip f a b -> String
showFlip = case forallC of
  Forall (Dict :: Dict (CompC Show (Flip f a) b)) -> show

fmapFlip :: forall f a a' b. (ForallC1 Functor f) => (a -> a') -> Flip f a b -> Flip f a' b
fmapFlip f = case forallC of
  Forall (Dict :: Dict (CompC Functor f b)) -> Flip . (fmap f) . runFlip

foldrFlip :: forall f a a' b. ForallC1 Foldable f => (a -> a' -> a') -> a' -> Flip f a b -> a'
foldrFlip f a' (Flip fba) =
  case forallC of
    Forall (Dict :: Dict (CompC Foldable f b)) -> foldr f a' fba

foldlFlip :: forall f a a' b. ForallC1 Foldable f => (a' -> a -> a') -> a' -> Flip f a b -> a'
foldlFlip f a' (Flip fba) =
  case forallC of
    Forall (Dict :: Dict (CompC Foldable f b)) -> foldl f a' fba

sequenceAFlip :: forall f a b t. (ForallC1 Traversable t, Applicative f) => Flip t (f a) b -> f (Flip t a b)
sequenceAFlip (Flip fba) =
  case forallC of
    Forall (Dict :: Dict (CompC Traversable t b)) -> Flip <$> sequenceA fba

newtype FlipNP f bs a = FlipNP { runFlipNP :: NP (Flip f a) bs }

liftA_FNP :: (forall b. f b a -> g b a) -> FlipNP f bs a -> FlipNP g bs a
liftA_FNP f = FlipNP . liftA_NP' (Flip . f . runFlip) . runFlipNP

liftA2_FNP :: forall f g h a bs. (forall b. f b a -> g b a -> h b a) -> FlipNP f bs a -> FlipNP g bs a -> FlipNP h bs a
liftA2_FNP f (FlipNP xs) (FlipNP ys) = FlipNP $ liftA2_NP' f' xs ys
  where
    f' :: forall b. Flip f a b -> Flip g a b -> Flip h a b
    f' (Flip x) (Flip y) = Flip $ f x y

collapse_FNP :: FlipNP (FK f) bs a -> [f a]
collapse_FNP (FlipNP np) = collapse_NP $ liftA_NP' f np
  where f (Flip (FK fa)) = K fa

instance (SListI bs, All (CompC Show (Flip f a)) bs) => Show (FlipNP f bs a) where
  show (FlipNP np) = showAsList $ collapse_NP $ cliftA_NP (Proxy::Proxy (CompC Show (Flip f a))) (K . show) np

instance ForallC1 Functor f => Functor (FlipNP f bs) where
  fmap f = FlipNP . liftA_NP' (fmapFlip f) . runFlipNP

instance ForallC1 Foldable f => Foldable (FlipNP f bs) where
  foldr f a' (FlipNP Nil) = a'
  foldr f a' (FlipNP (x :* xs)) = foldrFlip f (foldr f a' (FlipNP xs)) x

  foldl f a' (FlipNP Nil) = a'
  foldl f a' (FlipNP (x :* xs)) = foldl f (foldlFlip f a' x) (FlipNP xs)

instance (ForallC1 Functor t, ForallC1 Foldable t, ForallC1 Traversable t) => Traversable (FlipNP t bs) where
  sequenceA (FlipNP np) = fmap FlipNP $ sequence'_NP $ liftA_NP' (Comp . sequenceAFlip) np

newtype FlipNS f bs a = FlipNS { runFlipNS :: NS (Flip f a) bs }

liftA_FNS :: (forall b. f b a -> g b a) -> FlipNS f bs a -> FlipNS g bs a
liftA_FNS f = FlipNS . liftA_NS' (Flip . f . runFlip) . runFlipNS

liftA2_FNS :: forall f g h a bs. (forall b. f b a -> g b a -> h b a) -> FlipNP f bs a -> FlipNS g bs a -> FlipNS h bs a
liftA2_FNS f (FlipNP xs) (FlipNS ys) = FlipNS $ liftA2_NS' f' xs ys
  where
    f' :: forall b. Flip f a b -> Flip g a b -> Flip h a b
    f' (Flip x) (Flip y) = Flip $ f x y

collapse_FNS :: FlipNS (FK f) bs a -> f a
collapse_FNS (FlipNS ns) = collapse_NS $ liftA_NS' f ns
  where f (Flip (FK fa)) = K fa

instance (ForallC1 Show (Flip f a)) => Show (FlipNS f bs a) where
  show (FlipNS (Z x)) = showFlip x
  show (FlipNS (S xs)) = show (FlipNS xs)

instance ForallC1 Functor f => Functor (FlipNS f bs) where
  fmap f = FlipNS . liftA_NS' (fmapFlip f) . runFlipNS

instance ForallC1 Foldable f => Foldable (FlipNS f bs) where
  foldr f a' (FlipNS (S xs)) = foldr f a' (FlipNS xs)
  foldr f a' (FlipNS (Z x)) = foldrFlip f a' x

  foldl f a' (FlipNS (S xs)) = foldl f a' (FlipNS xs)
  foldl f a' (FlipNS (Z x)) = foldlFlip f a' x

instance (ForallC1 Functor t, ForallC1 Foldable t, ForallC1 Traversable t) => Traversable (FlipNS t bs) where
  sequenceA (FlipNS ns) = fmap FlipNS $ sequence'_NS $ liftA_NS' (Comp . sequenceAFlip) ns

newtype FlipSOP f bss a = FlipSOP { runFlipSOP :: FlipNS (FlipNP f) bss a }
--newtype FlipSOP f bss a = FlipSOP { runFlipSOP :: SOP (Flip f a) bss }

liftA_FSOP :: (forall b. f b a -> g b a) -> FlipSOP f bss a -> FlipSOP g bss a
liftA_FSOP f (FlipSOP sop) = FlipSOP (liftA_FNS (liftA_FNP f) sop)

collapse_FSOP :: FlipSOP (FK f) bs a -> [f a]
collapse_FSOP (FlipSOP sop) = unComp . collapse_FNS $ liftA_FNS (FK . Comp . collapse_FNP) sop

