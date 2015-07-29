module HLinear.PLE.Hook
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Math.Structure

import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.PLE.Hook.RPermute as RP

import HLinear.Matrix ( Matrix )
import qualified HLinear.Matrix as M


data PLEHook a =
  PLEHook
  { _permutation :: RPermute
  , _left        :: LeftTransformation a
  , _echelon     :: EchelonForm a
  }
  deriving Show


toMatrices
  :: ( DecidableZero a, DivisionRing a )
  => PLEHook a
  -> ( Matrix a, Matrix a, Matrix a )
toMatrices (PLEHook p l e) =
  ( RP.toMatrix $ recip p
  , LT.toInverseMatrix l
  , EF.toMatrix e
  )
