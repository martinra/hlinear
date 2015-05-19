{-# LANGUAGE
    TypeFamilies
  , GADTs
  , ScopedTypeVariables
  #-}

module HLinear.PLE.PLE
where

import Data.Permute
import HLinear.PLE.Hook.ReversePermute


data PLEHook m where
  PLEHook :: HasPLE m
          => PLEPermute m -> PLELeft m -> PLEEchelon m
          -> PLEHook m
newtype PLEDecomposition m = PLEDecomposition (PLEHook m)

_permutation :: HasPLE m => PLEHook m -> PLEPermute m
_permutation (PLEHook p _ _) = p

_left :: HasPLE m => PLEHook m -> PLELeft m
_left (PLEHook _ l _) = l

_echelon :: HasPLE m => PLEHook m -> PLEEchelon m
_echelon (PLEHook _ _ e) = e


newtype VVMatrixPermute m = VVMatrixPermute Permute

fromVVMatrixPermute :: VVMatrixPermute m -> Permute
fromVVMatrixPermute (VVMatrixPermute p) = p


class HasPLE m where
  type PLEPermute m :: *
  type PLELeft m :: *
  type PLEEchelon m :: *

  ple :: m -> PLEDecomposition m

  permutation :: PLEDecomposition m -> VVMatrixPermute m
  permutation (PLEDecomposition hook) = fromPLEPermute $ _permutation hook

  left :: PLEDecomposition m -> m
  left (PLEDecomposition hook) = fromPLELeft $ _left hook

  echelon :: PLEDecomposition m -> m
  echelon (PLEDecomposition hook) = fromPLEEchelon $ _echelon hook

  fromPLEPermute :: PLEPermute m -> VVMatrixPermute m
  fromPLELeft :: PLELeft m -> m
  fromPLEEchelon :: PLEEchelon m -> m
