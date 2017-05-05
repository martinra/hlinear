{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeFamilies
  , UndecidableInstances
  #-}

module HLinear.NormalForm.Instance.PLE
where

import Prelude ()
import HLinear.Utility.Prelude

import HFlint.FMPQ

import HLinear.NormalForm.PLE
import HLinear.NormalForm.FoldUnfold.PLE.DivisionRing as FUDR
import HLinear.NormalForm.FoldUnfold.PLE.FractionFree as FUFF


instance {-# OVERLAPPABLE #-}
     ( DivisionRing a, DecidableZero a, DecidableUnit a, MultiplicativeGroup (Unit a) )
  => HasPLE a
  where
    ple = FUDR.ple

instance {-# OVERLAPPING #-}
  HasPLE FMPQ
  where
    ple = FUFF.ple
