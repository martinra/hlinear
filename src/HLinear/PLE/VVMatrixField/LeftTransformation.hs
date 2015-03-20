{-# LANGUAGE
    ScopedTypeVariables
  #-}

module HLinear.PLE.VVMatrixField.LeftTransformation
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Applicative ( (<$>) )
import Data.Maybe
import Data.Vector ( Vector(..), (!) )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.VVMatrix.Basic ( cmbDim )
import HLinear.VVMatrix.Definition ( VVMatrix(..) )


 -- \ A vector of columns (a1^-1, [-ai]) which are offset by their index.
 --   Writing a for the first entry and v for entries of the column vector,
 --   it represents the matrix of the form
 --   a^-1 0    0    0
 --   -v   a^-1 0    0
 --   -v   -v   a^-1 0 
 --   -v   -v   -v   a^-1
 --   . . . .
data LeftTransformation a =
  LeftTransformation Natural (Vector (NonZero a, Vector a))

toVVMatrix :: forall a . Field a
           => LeftTransformation a -> VVMatrix a
toVVMatrix (LeftTransformation nrs cs) = 
  VVMatrix nrs nrs $
    V.generate (fromIntegral nrs) $ \ix ->
    V.generate (fromIntegral nrs) $ \jx ->
      case compare ix jx of
        LT -> zero
        EQ -> if jx < ncs
              then fromNonZero $ recip $ fst $ cs ! jx
              else one
        GT -> if jx < ncs
              then negate $ (snd $ cs ! jx) ! (ix-jx-1)
              else zero
  where
  ncs = V.length cs


nmbRows :: LeftTransformation a -> Natural
nmbRows (LeftTransformation nrs _) = nrs

nmbCols :: LeftTransformation a -> Natural
nmbCols (LeftTransformation nrs _) = nrs


concat :: LeftTransformation a -> LeftTransformation a
       -> LeftTransformation a
concat (LeftTransformation nrs cs) (LeftTransformation nrs' cs') =
  fromJust $ cmbDim nrs (nrs' + fromIntegral (V.length cs)) >>
  return ( LeftTransformation nrs $ cs V.++ cs' )


apply :: Field a
      => LeftTransformation a -> VVMatrix a
      -> VVMatrix a
apply (LeftTransformation nrs cs) (VVMatrix nrs' ncs' rs) =
  fromJust $ cmbDim nrs' nrs >>
  return ( VVMatrix nrs' ncs' rsL )
  where
  ncs = V.length cs

  rsScaled = V.zipWith ( \(pivotRecip,_) r ->
                         V.map (fromNonZero pivotRecip *) r )
                       cs rs
  rsL = V.generate (fromIntegral nrs) $ \ix ->
          V.foldl' (V.zipWith (+))
            ( if ix < ncs then rsScaled ! ix else rs ! ix )
            ( V.zipWith (\(_,c) r -> V.map ((c ! (ix-1)) *) r)
                (V.take (ix-1) cs) rsScaled )
