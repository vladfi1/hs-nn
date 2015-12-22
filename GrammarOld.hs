{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
--{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators, DataKinds, KindSignatures, PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grammar where

import Data.Proxy

import GHC.TypeLits
import GHC.Prim

import Data.Vinyl

data ConstraintList (c :: k -> Constraint) (l :: [k]) where
  TNil :: ConstraintList c '[]
  TCons :: c a => ConstraintList c l -> ConstraintList c (a ': l)

class ConstrainAll (c :: k -> Constraint) (l :: [k]) where
  reify :: ConstraintList c l

instance ConstrainAll c '[] where
  reify = TNil

instance (c a, ConstrainAll c l) => ConstrainAll c (a ': l) where
  reify = TCons reify

-- only nullary Type constructors
class (KnownSymbol s, ConstrainAll Constructor (Constructors s)) => Type s where
  type Constructors s :: [Symbol]
  constructors :: proxy s -> ConstraintList Constructor (Constructors s)
  constructors _ = reify

class (KnownSymbol s, ConstrainAll Type (Types s)) => Constructor (s :: Symbol) where
  type Types s :: [Symbol]
  types :: proxy s -> ConstraintList Type (Types s)
  types _ = reify

-- values
type Product = Rec

data Sum (f :: k -> *) (l :: [k]) where
  First :: f a -> Sum f (a ': l)
  Next :: Sum f l -> Sum f (a ': l)

data Value s where
  Value :: Type s => Sum Cons (Constructors s) -> Value s

data Cons s where
  Cons :: Constructor s => Product Value (Types s) -> Cons s

instance Show (Value s) where
  show (Value sum) = f proof sum where
    proof = reify :: ConstraintList Constructor (Constructors s)
    
    f :: ConstraintList Constructor l -> Sum Cons l -> String
    f (TCons _) (First cons) = symbolVal cons ++ show cons
    f (TCons proof') (Next sum') = f proof' sum'

instance Show (Cons s) where
  show (Cons prod) = f prod where
    f :: Product Value l -> String
    f RNil = ""
    f (val :& prod') = " (" ++ show val ++ ")" ++ f prod'

-- classes to make writing values easier

class Contains (l :: [k]) (a :: k) where
  contain :: f a -> Sum f l

instance Contains (a ': l) a where
  contain = First

instance {-# OVERLAPPABLE #-} Contains l a => Contains (t ': l) a where
  contain = Next . contain

#define P(s) (Proxy::Proxy (s))

type family Curried (f :: k -> *) (l :: [k]) (r :: *) where
  Curried f '[] r = r
  Curried f (a ': l) r = f a -> Curried f l r

class Curry (l :: [k]) where
  curry' :: (Rec f l -> a) -> Curried f l a

instance Curry '[] where
  curry' f = f RNil

instance Curry l => Curry (a ': l) where
  curry' f fa = curry' (\args -> f $ fa :& args)

-- example ADT/CFG
-- could reduce verbosity with Template Haskell
instance Type "Nat" where
  type Constructors "Nat" = '["Z", "S"]

instance Constructor "Z" where
  type Types "Z" = '[]

instance Constructor "S" where
  type Types "S" = '["Nat"]


#define C(s) (curry' (Value . contain . (Cons :: Product Value (Types "s") -> Cons "s")))

zero :: Value "Nat"
zero = C(Z)

-- cpphs buggy? doesn't work if C(S) C(Z) are on the same line
one :: Value "Nat"
one = C(S)
      C(Z)

two :: Value "Nat"
two = C(S) one

