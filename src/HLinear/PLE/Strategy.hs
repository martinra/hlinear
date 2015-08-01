{-# LANGUAGE
    FlexibleContexts
  , GADTs
  , RankNTypes
  , TypeFamilies
  #-}

module HLinear.PLE.Strategy
where

import HLinear.Matrix ( Matrix )
import HLinear.PLE.FoldUnfold.Echelonize
import HLinear.PLE.MultiMod.Echelonize.Definition
import HLinear.PLE.Sliced.Echelonize.Definition


data PLEStrategy a where
  FoldUnfold
    :: HasPLEDecompositionFoldUnfold Matrix a
    => PLEStrategy a
  Sliced
    :: HasPLEDecompositionSliced Matrix a
    => PLEDecompositionSlicedParameters a -> PLEStrategy a
  MultiMod
    :: HasPLEDecompositionMultiMod Matrix a
    => PLEDecompositionMultiModParameters a -> PLEStrategy a
-- | PAdic Natural
