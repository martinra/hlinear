{-# LANGUAGE
    FlexibleContexts
  , UndecidableInstances
  #-}

module HLinear.Hook.Algebra
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Permute ( Permute )
import qualified Data.Vector as V
import Math.Structure

import HLinear.Hook.Definition
import HLinear.Utility.RPermute ()
import HLinear.Hook.EchelonForm ()
import HLinear.Hook.LeftTransformation ()


instance ( Ring a, MultiplicativeGroup (Unit a) )
  => MultiplicativeMagma (PLEHook a)
  where
  (PLEHook p lt ef) * (PLEHook p' lt' ef') =
    PLEHook (p'*p) (lt' * (p' *. lt)) (ef + ef')

instance ( Ring a, MultiplicativeGroup (Unit a) )
  => MultiplicativeSemigroup (PLEHook a)
