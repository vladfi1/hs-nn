{-# LANGUAGE DataKinds, KindSignatures, ConstraintKinds, TypeFamilies #-}

-- minimal common interface between HMatrix and Accelerate
module GenericTensor where

import Data.Array.Accelerate

class Tensor (t :: [k] -> * -> *) where
  type C t :: * -> Constraint
  
  --tmap :: (C t a, C t b) => t '[n, m]
  transpose :: C t a => t '[n, m] a -> t '[m, n] a
  
  mv :: C t a => t '[n, m] a -> t '[m] a -> t '[n] a



