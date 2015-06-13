module HLinear.PLE.Hook.LeftTransformation.Conversion
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Applicative ( (<$>) )
import Control.Arrow ( first )
import Data.Maybe
import Data.Vector ( Vector(..) )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural ( Natural )


import HLinear.PLE.Hook.LeftTransformation.Basic
import HLinear.PLE.Hook.LeftTransformation.Column
import HLinear.PLE.Hook.LeftTransformation.Instances
import HLinear.PLE.Hook.RPermute
import HLinear.Matrix hiding ( (!), (!?) )
import HLinear.Matrix.Definition ( Matrix(..) )


fromVector :: DecidableZero a
             => Vector a -> LeftTransformation a
fromVector v = LeftTransformation nrs $ V.singleton $
                 LeftTransformationColumn 0 a c
  where
  nrs = fromIntegral $ V.length v
  a = nonZero $ V.head v
  c = V.tail v

toMatrix :: Ring a
          => LeftTransformation a -> Matrix a
toMatrix (LeftTransformation nrs cs) =
  Matrix nrs nrs $
    V.generate nrs' $ \ix ->
    V.generate nrs' $ \jx ->
      let a = maybe one ltcHead $ cs V.!? jx
      in
      case compare ix jx of
        LT -> zero
        EQ -> a
        GT -> maybe zero ((*a) . (!ix)) $ cs V.!? jx
  where
  nrs' = fromIntegral nrs

toInverseMatrix :: DivisionRing a
                => LeftTransformation a -> Matrix a
toInverseMatrix = toMatrix . recip
