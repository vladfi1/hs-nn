{-# LANGUAGE
  StandaloneDeriving,
  GeneralizedNewtypeDeriving
  #-}

module Identity where

import Data.Vinyl.Functor

deriving instance Num a => Num (Identity a)
deriving instance Fractional a => Fractional (Identity a)
deriving instance Floating a => Floating (Identity a)

