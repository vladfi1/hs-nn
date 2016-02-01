{-# LANGUAGE PolyKinds #-}

module Main where

import TensorAccelerate
import TensorDAG
import Data.Proxy

main = test (Proxy :: Proxy (ATensor Float))
