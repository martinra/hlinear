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
import HLinear.PLE.Decomposition.Definition
import HLinear.PLE.FoldUnfold.Echelonize.Definition
import HLinear.PLE.Strategy.Definition


instance HasPLEStrategy Identity (Matrix FMPQ) where
  data PLEStrategy Identity (Matrix FMPQ) where
    PLEStrategyFMPQFoldUnfold 
      :: HasPLEDecompositionFoldUnfold (Matrix FMPQ)
      => PLEStrategy Identity (Matrix FMPQ)
  
  dispatchPLEStrategy PLEStrategyFMPQFoldUnfold
    = Identity . pleDecompositionFoldUnfold
