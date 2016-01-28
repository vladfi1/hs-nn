{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module DAGIO where

import Data.IORef

import Data.Vinyl
import Data.Vinyl.Functor
import VinylUtils

import Control.Applicative hiding (Const)
import Control.Monad
import Data.Traversable

import VarArgs
import Gradients

import Data.Default
import DefaultM

data Some f where
  Some :: f a -> Some f

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
  output <- newIORef (Identity a)
  updated <- newIORef True
  gradOutput <- newIORef 0
  return Node
    { forward = \RNil -> Identity a
    , inputs = RNil
    , output = output
    , updated = updated
    , backwards = \RNil _ -> RNil
    , gradOutput = gradOutput
    }

instance (Num a, Default a) => DefaultM IO (Node a) where
    defM = makeSource def

makeNode :: (Num output) => Curry Node inputs (IO (Node output)) c => GradFunc inputs output -> c
makeNode GradFunc{..} = VarArgs.curry f where
  f inputs = do
    ins <- rtraverse readNode inputs
    output <- newIORef (function ins)
    updated <- newIORef True
    gradOutput <- newIORef 0
    return Node
      { forward = function
      , inputs = inputs
      , output = output
      , updated = updated
      , backwards = gradient
      , gradOutput = gradOutput
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
    unless (rnull inputs) $ do
      ins <- rtraverse evalNode' inputs
      writeIORef output (forward ins)
    writeIORef updated True
  readIORef output

evalNode node = getIdentity <$> evalNode' node

type Tape = [Some Node]

-- records the nodes traversed, in reverse order
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

{-
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
-}

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

