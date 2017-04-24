module HLinear.Hook.ERHook.Algebra
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Math.Structure

import HLinear.Hook.ERHook.Definition
import qualified HLinear.Hook.EchelonForm as EF
import qualified HLinear.Matrix as M


instance Ring a => MultiplicativeMagma (ERHook a) where
  (ERHook et m ef) * (ERHook et' m' ef') =
    ERHook
      (et'*et)
      (M.blockSumRows m' mTop)
      (EF.blockSumHook ef' mBottom ef)
    where
      (mTop,mBottom) = M.splitAtCols (fromIntegral $ M.nmbRows m') (et'*.m)

instance Ring a => MultiplicativeSemigroup (ERHook a)
