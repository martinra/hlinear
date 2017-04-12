{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HLinear.PLE.Decomposition.Instances
where

import Control.Monad.Identity
import Math.Structure ( DecidableZero, DivisionRing )
import HFlint.FMPQ

import HLinear.Matrix ( Matrix )
import HLinear.PLE.Decomposition.Definition
import HLinear.PLE.Strategy.FMPQ
import HLinear.PLE.Strategy.NMod
import HLinear.PLE.FoldUnfold
import HLinear.PLE.Strategy.Definition


instance {-# OVERLAPPABLE #-}
     ( DecidableZero a, DivisionRing a )
  => HasPLEDecomposition (Matrix a)
  where
  pleDecomposition = pleDecompositionFoldUnfold
