{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HLinear.PLE.Hook.PLMatrix
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Vector ( Vector )

import HLinear.Matrix ( Matrix )


newtype PLVector a = PLVector {fromPLVector :: Vector a}
newtype PLMatrix a = PLMatrix {fromPLMatrix :: Matrix a}
