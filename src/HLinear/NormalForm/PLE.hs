{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeFamilies
  , UndecidableInstances
  #-}

module HLinear.NormalForm.PLE
where

import HFlint.FMPQ
import Math.Structure ( DivisionRing, DecidableZero, DecidableUnit, MultiplicativeGroup, Unit )
import HLinear.Hook.PLEHook ( PLEHook(..) )
import HLinear.Matrix ( Matrix )
import qualified HLinear.PLE.FoldUnfold.FractionFree as FUFF
import qualified HLinear.PLE.FoldUnfold.DivisionRing as FUDR


class HasPLE a where
  type PLE a :: *

  ple :: a -> PLE a


instance {-# OVERLAPPABLE #-}
     ( DivisionRing a, DecidableZero a, DecidableUnit a, MultiplicativeGroup (Unit a) )
  => HasPLE (Matrix a)
  where
    type PLE (Matrix a) = PLEHook a
    ple = FUDR.ple

instance {-# OVERLAPPING #-}
  HasPLE (Matrix FMPQ)
  where
    type PLE (Matrix FMPQ) = PLEHook FMPQ
    ple = FUFF.ple
