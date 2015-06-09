module HLinear.VVMatrix.Monoid
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import Data.Monoid
import qualified Data.Vector as V
import Math.Structure

import HLinear.VVMatrix.Definition ( VVMatrix(..) )
import HLinear.VVMatrix.Force

-- block diagonal sums
instance    AdditiveMonoid a
         => Monoid (VVMatrix a) where
  mempty = Zero (Just 0) (Just 0)
  (VVMatrix nrs ncs rs) `mappend` (VVMatrix nrs' ncs' rs') =
    VVMatrix (nrs+nrs') (ncs+ncs') $ (V.++)
      ( V.map (V.++ zeros') rs )
      ( V.map (zeros V.++) rs' )
    where
      zeros = V.replicate (fromIntegral nrs) zero 
      zeros' = V.replicate (fromIntegral nrs') zero 
  mappend m m' = mappend (forceVV m) (forceVV m')
