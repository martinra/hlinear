{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , RankNTypes
  #-}

module HLinear.PLE.MultiMod.Echelonize.Definition
where

import Control.Monad.Trans.Maybe ( MaybeT )
import Data.Proxy ( Proxy )
import HFlint.FMPQ
import HFlint.NMod

import HLinear.MultiMod ( ReconstructionParameters )
import HLinear.PLE.Decomposition.Definition
import HLinear.PLE.Strategy.Definition


class HasPLEDecompositionFMPQMultiMod f where
  pleDecompositionMultiMod
    :: PLEDecompositionMultiModParameters
    -> (    forall ctx
         .  ReifiesNModContext ctx
         => Proxy ctx -> PLEStrategy Maybe (f (NMod ctx)) )
    -> f FMPQ -> PLEDecomposition (f FMPQ)

newtype PLEDecompositionMultiModParameters =
  PLEDecompositionMultiModParameters
  ReconstructionParameters
