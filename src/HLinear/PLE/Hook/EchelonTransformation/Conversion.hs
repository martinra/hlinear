module HLinear.PLE.Hook.EchelonTransformation.Conversion
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Maybe
import Data.Vector ( Vector(..) )
import qualified Data.Vector as V
import Math.Structure

import HLinear.PLE.Hook.EchelonTransformation.Algebra ()
import HLinear.PLE.Hook.EchelonTransformation.Basic
import HLinear.PLE.Hook.EchelonTransformation.Column
import HLinear.Matrix hiding ( (!), (!?) )
import HLinear.Matrix.Definition ( Matrix(..) )


toMatrix :: Ring a
          => EchelonTransformation a -> Matrix a
toMatrix (EchelonTransformation nrs cs) =
  Matrix nrs nrs $
    V.generate nrsZ $ \ix ->
    V.generate nrsZ $ \jx ->
      case compare ix jx of
        LT -> maybe zero (!ix) $ cs V.!? (nrsZ-1-jx)
        EQ -> one
        GT -> zero
  where
  nrsZ = fromIntegral nrs

toInverseMatrix :: ( DivisionRing a, DecidableZero a )
                => EchelonTransformation a -> Matrix a
toInverseMatrix = toMatrix . recip
