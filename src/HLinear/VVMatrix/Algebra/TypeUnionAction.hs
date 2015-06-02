{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  #-}

module HLinear.VVMatrix.Algebra.TypeUnionAction
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import Data.Composition ( (.:) )
import Data.Maybe ( fromJust )
import Data.Proxy ( Proxy(..) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Math.Structure
import GHC.TypeLits ( Nat, KnownNat, natVal )

import HLinear.VVMatrix.Algebra.TypeUnion
import HLinear.VVMatrix.Basic
import HLinear.VVMatrix.Creation
import HLinear.VVMatrix.Definition
import HLinear.VVMatrix.Utils


instance    ( Rng a, LinearSemiringLeftAction a b )
         => MultiplicativeSemigroupLeftAction
              (VVMatrix a) (Vector b)
  where
  (VVMatrix nrs ncs rs) *. v
    | fromIntegral nrs /= V.length v = error "VVMatrix a *. Vector a: Incompatible dimensions"
    | otherwise = V.foldl1 (+) $
                  (`V.map` rs) $ \r ->
                  V.zipWith (*.) r v
