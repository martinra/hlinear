{-# LANGUAGE UndecidableInstances #-}

module HLinear.NormalForm.Instance.RREF
where

import Prelude ()
import HLinear.Utility.Prelude

import HLinear.Hook.PLEHook ( PLEHook(..), PLREHook(..) )
import HLinear.NormalForm.PLE ( ple, HasPLE )
import HLinear.NormalForm.RREF ( HasRREF(..) )
import qualified HLinear.Hook.PLEHook as H
import qualified HLinear.NormalForm.FoldUnfold.RREF.DivisionRing as DR


instance {-# OVERLAPPABLE #-}
     ( DivisionRing a, DecidableZero a, DecidableUnit a, MultiplicativeGroup (Unit a), HasPLE a )
  => HasRREF a
  where
    rref m =
      let h@(PLEHook p l e) = ple m
          H.RREF r e' = DR.rref e
      in  PLREHook p l r e'
