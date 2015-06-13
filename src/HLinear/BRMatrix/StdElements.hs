module HLinear.BRMatrix.StdElements
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Composition ( (.:) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural

import HLinear.BRMatrix.Basic
import HLinear.BRMatrix.Definition
import HLinear.BRMatrix.RVector ( RVector(..) )
import qualified HLinear.BRMatrix.RVector as RV
import HLinear.Matrix.Conversion
import qualified HLinear.Matrix.StdElements as M


zeroMatrix :: AdditiveMonoid a
           => Natural -> Natural -> BRMatrix a
zeroMatrix = toBRMatrix .: M.zeroMatrix

identityMatrix :: ( AdditiveMonoid a, MultiplicativeMonoid a )
               => Natural -> BRMatrix a
identityMatrix = toBRMatrix . M.identityMatrix

diagonalMatrix :: AdditiveMonoid a
               => Vector a -> BRMatrix a
diagonalMatrix = toBRMatrix . M.diagonalMatrix


-- block diagonal sums and the associated monoid structure

-- note: block sums are taken in the natural order of the liminv for BRMatrix
-- this is different from block sums of Matrix
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
