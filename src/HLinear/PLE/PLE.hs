{-# LANGUAGE
    TypeFamilies
  , GADTs
  #-}
module HLinear.PLE.PLE
where

import HLinear.PLE.ReversePermute


data PLEHook m where
  PLEHook :: HasPLE m
          => ReversePermute -> PLELeft m -> PLEEchelon m
          -> PLEHook m
newtype PLEDecomposition m = PLEDecomposition (PLEHook m)

_permutation :: HasPLE m => PLEHook m -> ReversePermute
_permutation (PLEHook p _ _) = p

_left :: HasPLE m => PLEHook m -> PLELeft m
_left (PLEHook _ l _) = l

_echelon :: HasPLE m => PLEHook m -> PLEEchelon m
_echelon (PLEHook _ _ e) = e


class HasPLE m where
  type PLELeft m :: *
  type PLEEchelon m :: *

  ple :: m -> PLEDecomposition m

  left :: PLEDecomposition m -> m
  left (PLEDecomposition hook) = left' $ _left hook

  echelon :: PLEDecomposition m -> m
  echelon (PLEDecomposition hook) = echelon' $ _echelon hook

  left' :: PLELeft m -> m
  echelon' :: PLEEchelon m -> m

