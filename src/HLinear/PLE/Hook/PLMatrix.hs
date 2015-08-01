{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module HLinear.PLE.Hook.PLMatrix
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Math.Structure

import qualified HLinear.PLE.Hook.RPermute as RP
import HLinear.PLE.Hook.RPermute ( RPermute(..) )
import HLinear.PLE.Hook.LeftTransformation ( LeftTransformation(..) )
import HLinear.Matrix ( Matrix(..), headRows, tailRows )
import HLinear.Matrix.Conversion ( fromBRMatrixUnsafe, toBRMatrix )


newtype PLMatrix a = PLMatrix {fromPLMatrix :: Matrix a}

instance MultiplicativeSemigroupLeftAction RPermute (PLMatrix a) where
  p *. PLMatrix (Matrix nrs ncs rs) = PLMatrix $
    Matrix nrs ncs $ RP.fromRPVector $ p *. RP.RPVector rs

instance
     ( DivisionRing a, DecidableZero a )
  => MultiplicativeSemigroupLeftAction (LeftTransformation a) (PLMatrix a)
  where
  lt *. PLMatrix m@(Matrix nrs ncs _) = PLMatrix $ 
    fromBRMatrixUnsafe nrs ncs $ lt *. toBRMatrix m
