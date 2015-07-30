{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HLinear.PLE.Decomposition.Instances
where

import Math.Structure ( DecidableZero, DivisionRing )
import HFlint.FMPQ

import HLinear.Matrix ( Matrix )
import HLinear.MultiMod ( ReconstructionParameters(..) )
import HLinear.PLE.Decomposition.Definition
import HLinear.PLE.Decomposition.Strategy
import HLinear.PLE.FoldUnfold
import HLinear.PLE.MultiMod
import HLinear.PLE.Strategy


instance {-# OVERLAPPABLE #-}
     ( DecidableZero a, DivisionRing a )
  => HasPLEDecomposition Matrix a
  where
  pleDecomposition = pleDecompositionFoldUnfold

instance {-# OVERLAPPING #-}
  HasPLEDecomposition Matrix FMPQ
  where
  pleDecomposition = pleDecompositionWithStrategy $
    MultiMod PLEDecompositionMultiModParameters
      { multiModInnerPLEDecomposition =
          pleDecompositionWithStrategy FoldUnfold
      , reconstructionParameters = ReconstructionParameters
        { bufferSize = 20
        , reconstructionSkip = 10
        }
      }
