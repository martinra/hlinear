{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeFamilies
  , UndecidableInstances
  #-}

module HLinear.NormalForm.Instance.PLH
where

import Prelude ()
import HLinear.Utility.Prelude

import HLinear.Hook.PLEHook ( PLEHook(..), PLREHook(..), RREF(..) )
import HLinear.NormalForm.FoldUnfold.PLH.EuclideanDomain as FUED
import HLinear.NormalForm.FoldUnfold.PLH.Normalization ( HasPLHNormalization )
import HLinear.NormalForm.FoldUnfold.RREF.EuclideanDomain ( rref )
import HLinear.NormalForm.PLH


instance {-# OVERLAPPABLE #-}
  ( EuclideanDomain a, DecidableZero a, MultiplicativeGroup (Unit a)
  , HasPLHNormalization a )
  => HasPLH a
  where
    plh m =
      let PLEHook p l e = FUED.plh m
          RREF r e' = rref e
      in  PLREHook p l r e'
