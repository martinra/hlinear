module HLinear.NormalForm.FoldUnfold.Pivot
where

import HLinear.Utility.Prelude

import qualified Data.Vector as V

import HLinear.Matrix.Definition ( Matrix(..) )
import HLinear.Utility.RPermute ( RPermute(..) )
import qualified HLinear.Utility.RPermute as RP


pivotNonZeroPermutation
  :: DecidableZero a
  => Matrix a -> Maybe RPermute
pivotNonZeroPermutation (Matrix nrs ncs rs) = do
  pIx <- V.findIndex (not . isZero . V.head) rs
  return $ RP.fromTransposition nrs (0,pIx)
