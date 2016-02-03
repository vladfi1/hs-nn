{-# LANGUAGE PolyKinds #-}

module Main where

import TensorAccelerate
import TensorDAG
import Data.Proxy
import qualified Data.Array.Accelerate.CUDA as CUDA

data CUDA

instance Backend CUDA where
  run _ = CUDA.run
  run1 _ = CUDA.run1

main = test (Proxy :: Proxy (ATensor CUDA Float))
