{-# LANGUAGE
    FlexibleContexts
  , TypeFamilies
  , UndecidableInstances
  #-}

module HLinear.NormalForm.PLH
where

import Math.Structure ( EuclideanDomain, DecidableZero, DecidableUnit, MultiplicativeGroup, Unit )

import HLinear.Matrix ( Matrix )
import HLinear.Hook.PLEHook ( PLEHook(..), PLREHook(..), RREF(..) )
import HLinear.NormalForm.FoldUnfold.RREF.EuclideanDomain ( rref )
import HLinear.NormalForm.FoldUnfold.PLH.EuclideanDomain as FUED
import HLinear.NormalForm.FoldUnfold.PLH.Normalization ( HasPLHNormalization )


class HasPLH a where
  type PLH a :: *

  plh :: a -> PLH a

instance {-# OVERLAPPABLE #-}
  ( EuclideanDomain a, DecidableZero a, MultiplicativeGroup (Unit a)
  , HasPLHNormalization a )
  => HasPLH (Matrix a)
  where
    type PLH (Matrix a) = PLREHook a
    plh m =
      let PLEHook p l e = FUED.plh m
          RREF r e' = rref e
      in  PLREHook p l r e'
