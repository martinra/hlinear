{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module HLinear.PLE.Sliced.Echelonize.Definition
where

import Control.Monad.Identity
import Numeric.Natural

import HLinear.PLE.Decomposition.Definition
import HLinear.PLE.Strategy.Definition


class HasPLEDecompositionSliced f a where
  pleDecompositionSliced
    :: PLEDecompositionSlicedParameters
    -> PLEStrategy Identity a
    -> f a -> PLEDecomposition a

-- class HasPLEDecompositionMaybeSliced f a where
--   pleDecompositionSlicedMaybe
--     :: PLEDecompositionSlicedParameters
--     -> PLEStrategy Maybe a
--     -> f a -> Maybe (PLEDecomposition a)


data PLEDecompositionSlicedParameters =
  PLEDecompositionSlicedParameters
  { slicingStrategy :: SlicingStrategy
  , slicingSizeNmb :: SlicingSizeNmb
  }

data SlicingStrategy =
    SlicingBalanced
  | SlicingUnBalanced

data SlicingSizeNmb =
    SlicingSize Natural
  | SlicingNmb Natural
