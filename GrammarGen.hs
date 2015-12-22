{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, ConstraintKinds, PolyKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module GrammarGen where

import Generics.SOP

import Random

class Gen a where
  gen :: MonadDiscrete w m => m a

instance (SListI l, All Gen l) => Gen (NP I l) where
  gen = case (sList :: SList l) of
    SNil -> return Nil
    SCons -> (:*) <$> (I <$> gen) <*> gen

instance {-# OVERLAPPABLE #-} (Generic a, All2 Gen (Code a)) => Gen a where
  gen = uniform sums >>= fmap (to . SOP)

sums :: forall w l m. (MonadDiscrete w m, All2 Gen l) => [m (NS (NP I) l)]
sums = case (sList :: SList l) of
  SNil -> []
  SCons -> (Z <$> gen) : map (fmap S) sums
