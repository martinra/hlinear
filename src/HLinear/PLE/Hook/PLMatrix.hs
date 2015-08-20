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

import qualified Data.Vector as V
import Math.Algebra.MonicExtension as Ext
import Math.Structure

import qualified HLinear.PLE.Hook.RPermute as RP
import HLinear.PLE.Hook.RPermute ( RPermute(..) )
import HLinear.PLE.Hook.LeftTransformation ( LeftTransformation(..) )
import HLinear.PLE.Hook.LeftTransformation.Weak ( WeakLeftTransformation(..) )
import qualified HLinear.PLE.Hook.LeftTransformation.Weak as WLT
import HLinear.Matrix ( Matrix(..), headRows, tailRows )
import HLinear.Matrix.Conversion ( fromBRMatrixUnsafe, toBRMatrix )
import HLinear.Matrix.Extension ( ExtMatrix(..) )
import qualified HLinear.BRMatrix.RVector as RV


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

apply
  :: DivisionRing a
  => WeakLeftTransformation a -> Matrix a -> Matrix a
apply wlt (Matrix nrs ncs rs) =
  Matrix nrs ncs $
  V.map RV.toCurrentVector $ RV.toCurrentVector $
  WLT.apply wlt $
  RV.RVector $ V.map RV.RVector rs
