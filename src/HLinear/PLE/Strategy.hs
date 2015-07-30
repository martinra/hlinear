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


data PLEStrategy a where
  FoldUnfold
    :: HasPLEDecompositionFoldUnfold Matrix a
    => PLEStrategy a
  MultiMod
    :: HasPLEDecompositionMultiMod Matrix a
    => PLEDecompositionMultiModParameters a -> PLEStrategy a
-- | Sliced Natural PLEStrategy
-- | PAdic Natural
