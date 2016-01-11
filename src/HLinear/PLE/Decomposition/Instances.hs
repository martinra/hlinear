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
import HLinear.MultiMod ( ReconstructionParameters(..) )
import HLinear.PLE.Decomposition.Definition
import HLinear.PLE.Strategy.FMPQ
import HLinear.PLE.Strategy.NMod
import HLinear.PLE.FoldUnfold
import HLinear.PLE.MultiMod
import HLinear.PLE.Sliced
import HLinear.PLE.Strategy.Definition


instance {-# OVERLAPPABLE #-}
     ( DecidableZero a, DivisionRing a )
  => HasPLEDecomposition (Matrix a)
  where
  pleDecomposition = pleDecompositionFoldUnfold

-- instance {-# OVERLAPPING #-}
--   HasPLEDecomposition (Matrix FMPQ)
--   where
--   pleDecomposition = runIdentity . dispatchPLEStrategy  strat
--     where
--       strat = PLEStrategyFMPQSliced
--               PLEDecompositionSlicedParameters
--                 { slicingStrategy = SlicingBalanced
--                 , slicingSizeNmb  = SlicingSize 64
--                 }
--               stratMM
-- 
--       stratMM = PLEStrategyFMPQMultiMod
--                 ( PLEDecompositionMultiModParameters
--                   ReconstructionParameters
--                   { bufferSize = 20
--                   , reconstructionSkip = 10
--                   }
--                 )
--                 ( const $ PLEStrategyNModLiftIdenity
--                           PLEStrategyNModFoldUnfold
--                 )
