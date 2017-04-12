module HLinear.PLE.Hook.Algebra
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Permute ( Permute )
import qualified Data.Vector as V
import Math.Structure

import HLinear.PLE.Hook.Definition
import HLinear.PLE.Hook.RPermute ()
import HLinear.PLE.Hook.EchelonForm ()
import HLinear.PLE.Hook.LeftTransformation ()


instance
     (DecidableZero a, DivisionRing a)
  => MultiplicativeMagma (PLEHook a)
  where
  (PLEHook p lt ef) * (PLEHook p' lt' ef') =
    PLEHook (p'*p) (lt' * (p' *. lt)) (ef + ef')

instance
     (DecidableZero a, DivisionRing a)
  => MultiplicativeSemigroup (PLEHook a)
