module HLinear.PLE.Hook.LeftTransformation.Basic
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


import HLinear.PLE.Hook.LeftTransformation.Column
import HLinear.PLE.Hook.ReversePermute
import HLinear.VVMatrix hiding ( (!), (!?) )
import HLinear.VVMatrix.Utils
import HLinear.VVMatrix.Definition ( VVMatrix(..) )

import Debug.Trace


 -- \ A vector of columns (a, [v]) which are offset by their index.
 --   It represents a transformation from the left
 --     a1     0     0   0
 --   v*a1    a2     0   0
 --   v*a1  v*a2    a3   0 
 --   v*a1  v*a2  v*a3  a4
 --   . . . .
 --
data LeftTransformation a =
  LeftTransformation Natural (Vector (LeftTransformationColumn a))
  deriving Show

nmbRows :: LeftTransformation a -> Natural
nmbRows (LeftTransformation nrs _) = nrs

nmbCols :: LeftTransformation a -> Natural
nmbCols (LeftTransformation _ cs) = fromIntegral $ V.length cs

ltDrop :: Int -> LeftTransformation a -> LeftTransformation a
ltDrop ix (LeftTransformation nrs cs) = LeftTransformation nrs' $ V.drop ix cs
  where
  nrs' = fromIntegral $ fromIntegral nrs - ix


ltFromVector :: DecidableZero a
             => Vector a -> LeftTransformation a
ltFromVector v = LeftTransformation nrs $ V.singleton $
                 LeftTransformationColumn 0 a c
  where
  nrs = fromIntegral $ V.length v
  a = nonZero $ V.head v
  c = V.tail v

 -- | in the PLE decomopition a left transformation corresponds to the inverse
 --   of the matrix given above
 --    a1^-1         0           0      0
 --   -a2^-1*v   a2^-1           0      0
 --   -a3^-1*v  -a3^-1*v   a3^-1        0
 --   -a4^-1*v  -a4^-1*v  -a4^-1*v  a4^-1

toMatrix :: Ring a
          => LeftTransformation a -> VVMatrix a
toMatrix (LeftTransformation nrs cs) =
  VVMatrix nrs nrs $
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
                => LeftTransformation a -> VVMatrix a
toInverseMatrix (LeftTransformation nrs cs) =
  VVMatrix nrs nrs $
    V.generate nrs' $ \ix ->
      let arecip = maybe one ltcHeadRecip $ cs V.!? ix
      in V.generate nrs' $ \jx ->
        case compare ix jx of
          LT -> zero
          EQ -> arecip
          GT -> maybe zero ((arecip*) . negate . (!ix)) $ cs V.!? jx
  where
  nrs' = fromIntegral nrs
