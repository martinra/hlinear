{-# LANGUAGE
    FlexibleContexts
  , MultiParamTypeClasses
  , RankNTypes
  #-}

module HLinear.PLE.MultiMod.Echelonize.Definition
where

import HFlint.NMod

import HLinear.Matrix ( Matrix )
import HLinear.MultiMod
import HLinear.PLE.Decomposition.Definition


class HasPLEDecompositionMultiMod f a where
  pleDecompositionMultiMod
    :: PLEDecompositionMultiModParameters a
    -> f a -> PLEDecomposition a


data PLEDecompositionMultiModParameters a =
  PLEDecompositionMultiModParameters
  { multiModInnerPLEDecomposition
      :: forall ctx
      .  ReifiesNModContext ctx
      => Matrix (NMod ctx) -> PLEDecomposition (NMod ctx)
  , reconstructionParameters :: ReconstructionParameters
  }
