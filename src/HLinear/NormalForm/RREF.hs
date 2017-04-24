{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeFamilies
  , UndecidableInstances
  #-}

module HLinear.NormalForm.RREF
where

import Math.Structure ( DivisionRing, DecidableZero, DecidableUnit, MultiplicativeGroup, Unit )
import qualified HFlint.FMPQMat as FMPQMat

import HLinear.Hook.EchelonForm ( EchelonForm )
import HLinear.Hook.PLEHook ( PLEHook(..), PLREHook(..) )
import HLinear.Matrix ( Matrix )
import HLinear.NormalForm.PLE ( ple )
import qualified HLinear.Hook.PLEHook as H
import qualified HLinear.NormalForm.FoldUnfold.RREF.DivisionRing as DR


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

    rref = DR.rref

instance HasRREF FMPQMat.FMPQMat where
  type RREF FMPQMat.FMPQMat = FMPQMat.FMPQMat

  rref = FMPQMat.rref
