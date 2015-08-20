{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , RankNTypes
  , TypeFamilies
  #-}

module HLinear.PLE.Strategy.FMPQ
where

import Control.Monad.Identity
import Control.Monad.Trans.Maybe ( MaybeT )
import Data.Proxy
import GHC.Exts ( Constraint )
import Numeric.Natural
import HFlint.FMPQ
import HFlint.NMod

import HLinear.Matrix ( Matrix )
import HLinear.MultiMod ( ReconstructionParameters )
import HLinear.PLE.Decomposition.Definition
import HLinear.PLE.FoldUnfold.Echelonize.Definition
import HLinear.PLE.MultiMod.Echelonize.Definition
import HLinear.PLE.Sliced.Echelonize.Definition
import HLinear.PLE.Strategy.Definition


instance HasPLEStrategy Identity (Matrix FMPQ) where
  data PLEStrategy Identity (Matrix FMPQ) where
    PLEStrategyFMPQFoldUnfold 
      :: HasPLEDecompositionFoldUnfold (Matrix FMPQ)
      => PLEStrategy Identity (Matrix FMPQ)
  
    PLEStrategyFMPQSliced
      :: HasPLEDecompositionSliced (Matrix FMPQ)
      => PLEDecompositionSlicedParameters
      -> PLEStrategy Identity (Matrix FMPQ) -> PLEStrategy Identity (Matrix FMPQ)
  
    PLEStrategyFMPQMultiMod
      :: HasPLEDecompositionFMPQMultiMod Matrix
      => PLEDecompositionMultiModParameters
      -> (    forall ctx
           .  ReifiesNModContext ctx
           => Proxy ctx -> PLEStrategy Maybe (Matrix (NMod ctx)) )
      -> PLEStrategy Identity (Matrix FMPQ)

--  PLEStrategyFMPQPAdic
--    :: PLEDecompositionPAdicParameters
--    -> (    ReifiesPAdicContext ctx
--         => Proxy ctx -> PLEStrategy (MaybeT m) (PAdic ctx) )
--    -> PLEStrategy m FMPQ

  dispatchPLEStrategy PLEStrategyFMPQFoldUnfold
    = Identity . pleDecompositionFoldUnfold
  dispatchPLEStrategy (PLEStrategyFMPQSliced param strat)
    = Identity . pleDecompositionSliced param strat
  dispatchPLEStrategy (PLEStrategyFMPQMultiMod param strat)
    = Identity . pleDecompositionMultiMod param strat
