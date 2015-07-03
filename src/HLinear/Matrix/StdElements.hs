module HLinear.Matrix.StdElements
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

import HLinear.Matrix.Basic
import HLinear.Matrix.Definition


zeroMatrix :: AdditiveMonoid a
           => Natural -> Natural -> Matrix a
zeroMatrix nrs ncs =
  Matrix nrs ncs $
    V.replicate (fromIntegral nrs) $
    V.replicate (fromIntegral ncs) zero

identityMatrix :: ( AdditiveMonoid a, MultiplicativeMonoid a )
               => Natural -> Matrix a
identityMatrix = diagonalMatrix . (`V.replicate` one) . fromIntegral

diagonalMatrix :: AdditiveMonoid a
               => Vector a -> Matrix a
diagonalMatrix ds =
  Matrix nrs nrs $
    (`V.imap` ds) $ \ix d ->
    V.generate nrsZ $ \jx ->
      if ix==jx then d else zero
  where
    nrsZ = V.length ds
    nrs = fromIntegral nrsZ

-- block sums and the associated monoid structure

blockSum :: (AdditiveMonoid a)
         => Matrix a -> Matrix a -> Matrix a
Matrix nrs ncs rs `blockSum` Matrix nrs' ncs' rs' =
  Matrix (nrs+nrs') (ncs+ncs') $ (V.++)
    ( V.map (V.++ zeros') rs )
    ( V.map (zeros V.++) rs' )
  where
    zeros = V.replicate (fromIntegral ncs) zero 
    zeros' = V.replicate (fromIntegral ncs') zero 

blockSumRows :: Matrix a -> Matrix a -> Matrix a
blockSumRows (Matrix nrs ncs rs) (Matrix nrs' ncs' rs')
  | nrs /= nrs' = error "Matrix.blockSumRows: unequal number of rows"
  | otherwise   = Matrix nrs (ncs+ncs') $
                    V.zipWith (V.++) rs rs'

blockSumCols :: Matrix a -> Matrix a -> Matrix a
blockSumCols (Matrix nrs ncs rs) (Matrix nrs' ncs' rs')
  | ncs /= ncs' = error "Matrix.blockSumCols: unequal number of cols"
  | otherwise   = Matrix (nrs+nrs') ncs $ rs V.++ rs'

blockMatrix :: Vector (Vector (Matrix a)) -> Matrix a
blockMatrix = V.foldl1' blockSumCols . V.map (V.foldl1' blockSumRows)

instance AdditiveMonoid a => Monoid (Matrix a) where
  mempty = zeroMatrix 0 0
  mappend = blockSum
