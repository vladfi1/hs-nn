{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module DAGIO where

import Data.IORef

import Data.Vinyl
import Data.Vinyl.Functor
import VinylUtils

import Control.Monad

import VarArgs

import Data.Default
import DefaultM

import Numeric.AD

--import qualified Algebra.Additive as Additive

import Prelude hiding (curry, uncurry)

data Some f where
  Some :: f a -> Some f

deriving instance Num a => Num (Identity a)

data Node (output :: *) where
  Node :: (Num output) =>
    { forward :: HList inputs -> Identity output
    , inputs :: Rec Node inputs
    , output :: IORef (Identity output)
    , updated :: IORef Bool
    , backwards :: HList inputs -> Identity output -> HList inputs
    , gradOutput :: IORef (Identity output)
    } -> Node output

makeSource :: (Num a) => a -> IO (Node a)
makeSource a = do
  output' <- newIORef (Identity a)
  updated' <- newIORef True
  gradOutput' <- newIORef 0
  return Node
    { forward = \RNil -> Identity a
    , inputs = RNil
    , output = output'
    , updated = updated'
    , backwards = \RNil _ -> RNil
    , gradOutput = gradOutput'
    }

instance (Num a, Default a) => DefaultM IO (Node a) where
    defM = makeSource def

-- these types are somewhat less general than would be ideal
makeUnary :: (Floating a) => (forall b. Floating b => b -> b) -> Node a -> IO (Node a)
makeUnary f = makeNode (uncurry1 f, \inputs output -> (output * uncurry1 (diff f) inputs) :& RNil)

makeBinary :: (Num a) => (forall b. Num b => b -> b -> b) -> Node a -> Node a -> IO (Node a)
makeBinary f = makeNode (uncurry2 f, g) where
  g (x :& y :& RNil) output = dx :& dy :& RNil
    where [dx, dy] = map (output *) $ grad (\[a, b] -> f a b) [x, y]

makeNode :: (Num output) => Curry Node inputs (IO (Node output)) c => (HList inputs -> Identity output, HList inputs -> Identity output -> HList inputs) -> c
makeNode (f, b) = curry g where
  g inputs' = do
    ins <- rtraverse readNode inputs'
    output' <- newIORef (f ins)
    updated' <- newIORef True
    gradOutput' <- newIORef 0
    return Node
      { forward = f
      , inputs = inputs'
      , output = output'
      , updated = updated'
      , backwards = b
      , gradOutput = gradOutput'
      }

readNode :: Node output -> IO (Identity output)
readNode Node{output} = readIORef output

resetNode :: Node output -> IO ()
resetNode Node{..} = do
  todo <- readIORef updated
  when todo $ do
    rtraverse_ (\n -> Const <$> resetNode n) inputs
    writeIORef gradOutput 0
    writeIORef updated False

evalNode' :: Node output -> IO (Identity output)
evalNode' node@Node{..} = do
  done <- readIORef updated
  unless done $ do
    ins <- rtraverse evalNode' inputs
    unless (rnull ins) $ writeIORef output (forward ins)
    writeIORef updated True
  readIORef output

evalNode node = getIdentity <$> evalNode' node

type Tape = [Some Node]

evalNodeTape :: IORef Tape -> Node output -> IO (Identity output)
evalNodeTape tape node@Node{..} = do
  done <- readIORef updated
  unless done $ do
    ins <- rtraverse (evalNodeTape tape) inputs
    unless (rnull ins) $ writeIORef output (forward ins)
    modifyIORef tape (Some node :)
    writeIORef updated True
  readIORef output

backprop tape = traverse f tape where
  writeGrad Node{gradOutput} grad = modifyIORef gradOutput (grad +)
  
  f (Some Node{..}) = do
    ins <- rtraverse readNode inputs
    out <- readIORef gradOutput
    let gradInputs = backwards ins out
    liftA2_ writeGrad inputs gradInputs

setLearningRate rate Node{..} = writeIORef gradOutput (Identity rate)

learn (Some Node{..}) = do
  grad <- readIORef gradOutput
  modifyIORef output (grad+)


testGrad = do
  param <- makeSource 0
  loss <- makeUnary id param
  
  tape <- newIORef []
  resetNode loss
  error <- evalNodeTape tape loss
  print error
  
  print =<< length <$> readIORef tape
  
  setLearningRate (-0.1) loss
  backprop =<< readIORef tape
  
  g <- readIORef $ gradOutput param
  print g


{- pull-based backprop scrapped in favor of tape version
resetGrad :: Node output -> IO ()
resetGrad Node{gradUpdated} = do
  todo <- readIORef gradUpdated
  when todo $ do
    children' <- readIORef children
    rtraverse_ (\n -> Const <$> resetGrad n) children'
    writeIORef gradOutput 0
    writeIORef gradUpdated False

backprop :: Some Node -> IO ()
backprop (Some Node{..}) = do
  done <- readIORef gradUpdated
  unless done $ do
    children' <- readIORef children
    traverse backprop children
    ins <- rtraverse readNode inputs
    out <- readIORef gradOutput
    let gradInputs = backwards ins out
    rZipWith inputs gradInputs _
-}

