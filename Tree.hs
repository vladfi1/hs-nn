{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Tree where

import Data.Foldable
import Data.Traversable
import Control.Applicative

import Control.Monad.Free
import Data.Functor.Compose

import Control.Monad.Writer

data Bin a = Bin a a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative Bin where
  pure x = Bin x x
  (Bin f g) <*> (Bin x y) = Bin (f x) (g y)

type BinTree = Free Bin

bin :: MonadFree Bin m => m a -> m a -> m a
bin l r = wrap (Bin l r)

--data Balanced a = Leaf a | Node (Balanced (Bin a))

newtype Balanced a = Balanced (Free (Compose Balanced Bin) a)

-- computes a fold (sum) and sets all leaves to that sum in one pass
foo :: Monoid a => BinTree a -> BinTree a
foo tree = t where
  helper s a = writer (s, a)
  (t, s) = runWriter $ traverse (helper s) tree

{- can do this with a dequeue
inOrder :: BinTree a -> [a]
inOrder 
  helper (todo, done) = 
-}
