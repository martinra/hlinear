module HLinear.Hook.ERHook.Algebra
where

import qualified Prelude as P
import HLinear.Utility.Prelude

import HLinear.Hook.ERHook.Definition
import qualified HLinear.Hook.EchelonForm as EF
import qualified HLinear.Matrix.Block as M


instance Ring a => MultiplicativeMagma (ERHook a) where
  {-# INLINABLE (*) #-}
  (ERHook et m ef) * (ERHook et' m' ef') =
    ERHook
      (et'*et)
      (M.blockSumRows m' mTop)
      (EF.blockSumHook ef' mBottom ef)
    where
      (mTop,mBottom) = M.splitAtCols (nmbRows m') (et'*.m)

instance Ring a => MultiplicativeSemigroup (ERHook a)
