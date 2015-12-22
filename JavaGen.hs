{-# OPTIONS_GHC -fcontext-stack=200 #-}

module JavaGen where

import GrammarGen
import JavaGeneric
import Random
import Language.Java.Syntax

instance Gen Int where
  gen = uniform [-1, 0, 1]

instance Gen Char where
  gen = uniform [' ' .. '~']

instance Gen Integer where
  gen = uniform [-1, 0, 1]

instance Gen Double where
  gen = return 0


genJava :: MonadDiscrete w m => m CompilationUnit
genJava = gen
