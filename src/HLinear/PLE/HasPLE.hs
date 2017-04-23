{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeFamilies
  , UndecidableInstances
  #-}

module HLinear.PLE.HasPLE
where

import HFlint.FMPQ
import qualified HFlint.FMPQMat as FMPQMat
import Math.Structure ( DivisionRing, DecidableZero, DecidableUnit, MultiplicativeGroup, Unit )
import HLinear.PLE.Hook.EchelonForm ( EchelonForm )
import HLinear.PLE.Hook.EchelonTransformation ( EchelonTransformation )
import HLinear.PLE.Hook ( PLEHook(..), PLREHook(..) )
import qualified HLinear.PLE.Hook as H
import HLinear.Matrix ( Matrix )
import qualified HLinear.PLE.FoldUnfold.FractionFree as FUFF
import qualified HLinear.PLE.FoldUnfold.DivisionRing as FUDR
import qualified HLinear.PLE.FoldUnfold.ReducedEchelonForm as REF



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


class HasRREF a where
  type RREF a :: *

  rref :: a -> RREF a

instance
     ( DivisionRing a, DecidableZero a )
  => HasRREF (PLEHook a)
  where
    type RREF (PLEHook a) = H.RREF a

    rref (PLEHook _ _ e) = rref e

instance
     ( DivisionRing a, DecidableZero a, DecidableUnit a, MultiplicativeGroup (Unit a) )
  => HasRREF (Matrix a)
  where
    type RREF (Matrix a) = PLREHook a

    rref m =
      let h@(PLEHook p l e) = ple m
          H.RREF r e' = rref h
      in  PLREHook p l r e'

instance
     ( DivisionRing a, DecidableZero a )
  => HasRREF (EchelonForm a)
  where
    type RREF (EchelonForm a) = H.RREF a

    rref = REF.reducedEchelonForm

instance HasRREF FMPQMat.FMPQMat where
  type RREF FMPQMat.FMPQMat = FMPQMat.FMPQMat

  rref = FMPQMat.rref
