{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module JavaDAG where

import Data.Vector.Storable (toList, (!))

import Language.Java.Syntax
import Language.Java.Parser
import Language.Java.Pretty

import Generics.SOP
import Generics.SOP.NP
import Data.Vinyl
import Data.Singletons.Prelude hiding (And)

import Data.Default
import DefaultM

import GrammarNNDAG
import JavaGeneric
import GHC.TypeLits
import GenericTensor
import TensorHMatrix
import TensorAccelerate
import TensorDAG
import DAGIO
import Random
import List
import Constraints as C

import Control.Monad
import Data.IORef

chars = [' ' .. '~']
numChars = length chars -- 95

-- TODO: use a Map
unsafeIndex :: Eq a => a -> [a] -> Int
unsafeIndex _ [] = error "Element not found."
unsafeIndex a (x:xs) =
  if a == x then 0
    else 1 + unsafeIndex a xs

data Java :: Nat -> *

type family JavaSize t where
  JavaSize Char = FromInteger 95
  JavaSize Int = FromInteger 1
  JavaSize Integer = FromInteger 1
  JavaSize Double = FromInteger 1
  JavaSize t = FromInteger 50

type instance Size Java t = JavaSize t

encodeParams :: (Tensor t) => IO (NP (EncodeParams t Java) GenericTypes)
encodeParams = sequence'_NP $ cpure_NP (Proxy::Proxy (And (KnownSize Java) (KnownSizes Java))) (Comp defM)

encodeChar :: forall t. Tensor t => Encoder t Java Char
encodeChar = case tensorFloat of (C.Dict :: C.Dict (Floating (t '[95]))) -> Encoder f
  where f c = Primitive . ReprT <$> makeSource (oneHot $ unsafeIndex c chars)

encodeInt :: forall t. Tensor t => Encoder t Java Int
encodeInt = case tensorFloat of (C.Dict :: C.Dict (Floating (t '[1]))) -> Encoder f
  where f i = Primitive . ReprT <$> makeSource (fromIntegral i)

encodeInteger :: forall t. Tensor t => Encoder t Java Integer
encodeInteger = case tensorFloat of (C.Dict :: C.Dict (Floating (t '[1]))) -> Encoder f
  where f i = Primitive . ReprT <$> makeSource (fromIntegral i)

encodeDouble :: forall t. Tensor t => Encoder t Java Double
encodeDouble = case tensorFloat of (C.Dict :: C.Dict (Floating (t '[1]))) -> Encoder f
  where f d = Primitive . ReprT <$> makeSource (realToFrac d)

primEncoders = encodeChar :& encodeInt :& encodeInteger :& encodeDouble :& RNil

testEncoding :: IO (Encoding (HTensor Float) Java CompilationUnit)
testEncoding = do
  java <- readFile "Test.java"
  let Right parsed = parser compilationUnit java

  params <- encodeParams

  let encoder = makeEncoder javaComplete params primEncoders

  runAnyEncoder encoder parsed

decodeParams :: (Tensor t) => IO (NP (DecodeParams t Java) GenericTypes)
decodeParams = sequence'_NP $ cpure_NP (Proxy::Proxy (And (KnownCode Java) (And (KnownSize Java) (KnownSizes Java)))) (Comp defM)

{-
decodeChar :: forall a. (Real a, Tensor t) => Decoder a Java Char
decodeChar = Decoder f where
  f :: ReprT a Java Char -> IO Char
  f c = do
    Vector v <- evalNode (runReprT c)
    sample $ zip chars (map toRational $ toList v)

decodeInt :: forall a. (RealFrac a, Tensor t) => Decoder a Java Int
decodeInt = Decoder f where
  f :: ReprT a Java Int -> IO Int
  f i = do
    Vector v <- evalNode (runReprT i)
    return . round $ v ! 0

decodeInteger :: forall a. (RealFrac a, Tensor t) => Decoder a Java Integer
decodeInteger = Decoder f where
  f :: ReprT a Java Integer -> IO Integer
  f i = do
    Vector v <- evalNode (runReprT i)
    return . round $ v ! 0

decodeDouble :: forall a. (Real a, Tensor t) => Decoder a Java Double
decodeDouble = Decoder f where
  f :: ReprT a Java Double -> IO Double
  f i = do
    Vector v <- evalNode (runReprT i)
    return . realToFrac $ v ! 0

primDecoders = decodeChar :& decodeInt :& decodeInteger :& decodeDouble :& RNil

javaDecoder :: IO (AnyDecoder Float Java AllTypes)
javaDecoder = do
  params <- decodeParams
  return $ makeDecoder javaComplete params primDecoders
-}

primAutoDecoders :: Tensor t => Rec (AutoDecoder t Java) PrimTypes
primAutoDecoders = x :& x :& x :& x :& RNil where x = primAutoDecoder

makeJavaAutoEncoder = do
  encodeParams' <- encodeParams
  decodeParams' <- decodeParams
  
  return $ makeAutoEncoder javaComplete encodeParams' primEncoders decodeParams' primAutoDecoders

main :: forall (t :: [Nat] -> *). (Tensor t, Show (t '[])) => Proxy t -> IO ()
main _ = do
  encodeParams' :: NP (EncodeParams t Java) GenericTypes <- encodeParams
  decodeParams' <- decodeParams
  
  let params = concat $ collapse_NP (liftA_NP (K . getNodes) encodeParams') ++ collapse_NP (liftA_NP (K . getNodes) decodeParams')
  
  --let decoder = makeDecoder javaComplete decodeParams' primDecoders
  
  let autoEncoder = makeAutoEncoder javaComplete encodeParams' primEncoders decodeParams' primAutoDecoders

  java <- readFile "Test.java"
  let Right parsed = parser compilationUnit java
  
  putStrLn $ prettyPrint parsed
  
  loss <- runAnyAutoEncoder autoEncoder parsed
  
  let train = do
        tape <- newIORef []
        resetNode loss
        error <- evalNodeTape tape loss
        print error
        
        case tensorFloat of (C.Dict :: C.Dict (Floating (t '[]))) -> setLearningRate (-0.001) loss
        backprop =<< readIORef tape
        traverse learn params

  traverse (const train) [1..1000]
  
  --j :: CompilationUnit <- runAnyDecoder decoder =<< defM
  --putStrLn $ prettyPrint j
  
  return ()

