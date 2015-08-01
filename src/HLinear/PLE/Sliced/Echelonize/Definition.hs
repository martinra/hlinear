{-# LANGUAGE
    FlexibleContexts
  , MultiParamTypeClasses
  , RankNTypes
  #-}

module HLinear.PLE.Sliced.Echelonize.Definition
where

import Data.Vector ( Vector )

import HLinear.Matrix ( Matrix )
import HLinear.PLE.Decomposition.Definition


class HasPLEDecompositionSliced f a where
  pleDecompositionSliced
    :: PLEDecompositionSlicedParameters a
    -> f a -> PLEDecomposition a


data PLEDecompositionSlicedParameters a =
  PLEDecompositionSlicedParameters
  { slicedInnerPLEDecomposition
      :: Matrix a -> PLEDecomposition a
  , slicedPositions :: Vector (Int,Int)
  }
