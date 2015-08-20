{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , RankNTypes
  , TypeFamilies
  #-}

module HLinear.PLE.Strategy.NMod
where

import Control.Monad.Identity
import Control.Monad.Trans.Maybe ( MaybeT )
import Data.Proxy
import GHC.Exts ( Constraint )
import Numeric.Natural
import HFlint.FMPQ
import HFlint.NMod

import HLinear.Matrix ( Matrix )
import HLinear.PLE.Decomposition.Definition
import HLinear.PLE.FoldUnfold.Echelonize.Definition
import HLinear.PLE.Sliced.Echelonize.Definition
import HLinear.PLE.Strategy.Definition


instance HasPLEStrategy Identity (Matrix (NMod ctx)) where
  data PLEStrategy Identity (Matrix (NMod ctx)) where
    PLEStrategyNModFoldUnfold 
      :: ( ReifiesNModContext ctx
         , HasPLEDecompositionFoldUnfold (Matrix (NMod ctx)) )
      => PLEStrategy Identity (Matrix (NMod ctx))
  
    PLEStrategyNModSliced
      :: ( ReifiesNModContext ctx
         , HasPLEDecompositionSliced (Matrix (NMod ctx)) )
      => PLEDecompositionSlicedParameters
      -> PLEStrategy Identity (Matrix (NMod ctx))
      -> PLEStrategy Identity (Matrix (NMod ctx))

  dispatchPLEStrategy PLEStrategyNModFoldUnfold
    = Identity . pleDecompositionFoldUnfold
  dispatchPLEStrategy (PLEStrategyNModSliced param strat)
    = Identity . pleDecompositionSliced param strat


instance HasPLEStrategy Maybe (Matrix (NMod ctx)) where
  data PLEStrategy Maybe (Matrix (NMod ctx)) where
    PLEStrategyNModLiftIdenity
      :: PLEStrategy Identity (Matrix (NMod ctx))
      ->  PLEStrategy Maybe (Matrix (NMod ctx))

  dispatchPLEStrategy (PLEStrategyNModLiftIdenity strat)
    = Just . runIdentity . dispatchPLEStrategy strat
