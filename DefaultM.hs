{-# LANGUAGE MultiParamTypeClasses #-}

module DefaultM where

class DefaultM m a where
  defM :: m a
