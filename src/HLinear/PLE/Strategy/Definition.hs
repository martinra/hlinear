{-# LANGUAGE
    MultiParamTypeClasses
  , TypeFamilies
  #-}

module HLinear.PLE.Strategy.Definition
where

import HLinear.Matrix ( Matrix )
import HLinear.PLE.Decomposition.Definition


class HasPLEStrategy m a where
  data PLEStrategy m a :: *

  dispatchPLEStrategy
    :: PLEStrategy m a -> Matrix a -> m (PLEDecomposition a)
