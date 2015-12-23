{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds, PolyKinds, KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeLevel where

type family Map (f :: k1 -> k2) (l :: [k1]) :: [k2] where
  Map f '[] = '[]
  Map f (e ': l) = (f e) ': (Map f l)

type family FoldR (f :: a -> b -> b) (end :: b) (l :: [a]) :: b
type instance FoldR f end '[] = end
type instance FoldR f end (e ': l) = f e (FoldR f end l)

type xs :++: ys = FoldR '(:) ys xs

type family Concat (xss :: [[k]]) :: [k] where
  Concat '[] = '[]
  Concat (xs ': xss) = xs :++: (Concat xss)

-- Undecidable instances?
-- ghc can't tell that FoldL is decreasing in its last argument
type family FoldL (f :: b -> a -> b) (acc :: b) (l :: [a]) :: b
type instance FoldL f acc '[] = acc
type instance FoldL f acc (e ': l) = FoldL f (f acc e) l

type family If (pred :: Bool) (a :: t) (b :: t) :: t where
  If False a b = b
  If True a b = a

-- kind-indexed
-- no way relate the kinds of the inputs and output
type family FMap (f :: a -> b) (c :: fa) :: fb where
  FMap f l = Map f l
  FMap f Nothing = Nothing
  FMap f (Just x) = Just (f x)

type family MaybeCase (m :: Maybe a) (n :: b) (f :: a -> b) :: b where
  MaybeCase Nothing n f = n
  MaybeCase (Just a) n f = f a

type family MaybeCase' (m :: Maybe a) (n :: Maybe b) (f :: a -> b) :: Maybe b where
  MaybeCase' Nothing n f = n
  MaybeCase' (Just a) n f = Just (f a)

type family MaybeBind (m :: Maybe a) (f :: a -> Maybe b) :: Maybe b where
  MaybeBind Nothing f = Nothing
  MaybeBind (Just a) f = f a
