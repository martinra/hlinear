module HLinear.NormalForm.FoldUnfold.Pivot
where

import HLinear.Utility.Prelude

import qualified Data.Vector as V

import HLinear.Matrix.Definition ( Matrix(..) )
import HLinear.NormalForm.FoldUnfold.Matrix ( splitOffTopLeft )
import HLinear.Utility.RPermute ( RPermute(..) )
import qualified HLinear.Utility.RPermute as RP


{-# INLINABLE pivotNonZeroPermutation #-}
pivotNonZeroPermutation
  :: DecidableZero a
  => Matrix a -> Maybe RPermute
pivotNonZeroPermutation (Matrix nrs ncs rs) = do
  pIx <- V.findIndex (not . isZero . V.head) rs
  return $ RP.fromTransposition nrs (0,pIx)

{-# INLINABLE splitOffPivotNonZero #-}
splitOffPivotNonZero
  :: DecidableZero a
  => Matrix a
  -> Maybe (RPermute, ((NonZero a, Vector a), (Vector a, Vector (Vector a))))
splitOffPivotNonZero m = do
  p <- pivotNonZeroPermutation m
  ((tl, tr), b) <- splitOffTopLeft (p *. m)
  return (p, ((NonZero tl, tr), b))
