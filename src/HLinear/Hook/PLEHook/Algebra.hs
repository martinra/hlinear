module HLinear.Hook.PLEHook.Algebra
where

import HLinear.Utility.Prelude

import Data.Permute ( Permute )
import qualified Data.Vector as V

import HLinear.Hook.EchelonForm ()
import HLinear.Hook.LeftTransformation ()
import HLinear.Hook.PLEHook.Definition
import HLinear.Utility.RPermute ()
import qualified HLinear.Hook.EchelonForm.Algebra as EF


instance Ring a => MultiplicativeMagma (PLEHook a) where
  {-# INLINABLE (*) #-}
  (PLEHook p lt ef) * (PLEHook p' lt' ef') =
    PLEHook (p'*p) (lt' * (p' *. lt)) (ef + ef')

instance Ring a => MultiplicativeSemigroup (PLEHook a)
