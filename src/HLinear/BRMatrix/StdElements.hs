module HLinear.BRMatrix.StdElements
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Vector ( Vector )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural

import HLinear.BRMatrix.Basic
import HLinear.BRMatrix.Definition
import HLinear.BRMatrix.RVector ( RVector(RVector) )
import qualified HLinear.BRMatrix.RVector as RV


zeroMatrix :: AdditiveMonoid a
           => Natural -> Natural -> BRMatrix a
zeroMatrix nrs ncs =
  BRMatrix nrs ncs $
    RVector $ V.replicate (fromIntegral nrs) $
    RVector $ V.replicate (fromIntegral ncs) zero

identityMatrix :: ( AdditiveMonoid a, MultiplicativeMonoid a )
               => Natural -> BRMatrix a
identityMatrix = diagonalMatrix . (`V.replicate` one) . fromIntegral

diagonalMatrix :: AdditiveMonoid a
               => Vector a -> BRMatrix a
diagonalMatrix ds =
  BRMatrix nrs nrs $
    RVector $ (`V.imap` ds) $ \ix d ->
    RVector $ V.generate nrsZ $ \jx ->
      if ix==jx then d else zero
  where
    nrsZ = V.length ds
    nrs = fromIntegral nrsZ

-- block diagonal sums and the associated monoid structure

-- block sums are taken in the natural order of the liminv for BRMatrix
blockSum :: (AdditiveMonoid a)
         => BRMatrix a -> BRMatrix a -> BRMatrix a
BRMatrix nrs ncs (RVector rs) `blockSum` BRMatrix nrs' ncs' (RVector rs') =
  BRMatrix (nrs+nrs') (ncs+ncs') $ RVector $ (V.++)
    ( V.map (RV.liftRV (V.++ zeros)) rs' )
    ( V.map (RV.liftRV (zeros' V.++)) rs )
  where
    zeros = V.replicate (fromIntegral nrs) zero 
    zeros' = V.replicate (fromIntegral nrs') zero 

instance AdditiveMonoid a => Monoid (BRMatrix a) where
  mempty = zeroMatrix 0 0
  mappend = blockSum
