module HLinear.Matrix.Block
where

import qualified Prelude as P
import HLinear.Utility.Prelude

import qualified Data.Vector as V

import HLinear.Matrix.Basic hiding ( zero )
import qualified HLinear.Matrix.Basic as M
import HLinear.Matrix.Definition


--------------------------------------------------------------------------------
-- block sums and the associated monoid structure
--------------------------------------------------------------------------------

blockSum :: (AdditiveMonoid a)
         => Matrix a -> Matrix a -> Matrix a
Matrix nrs ncs rs `blockSum` Matrix nrs' ncs' rs' =
  Matrix (nrs+nrs') (ncs+ncs') $
    ( fmap (<> zeros') rs ) <> ( fmap (zeros <>) rs' )
  where
    zeros = V.replicate ncs zero 
    zeros' = V.replicate ncs' zero 

blockSumRows :: Matrix a -> Matrix a -> Matrix a
blockSumRows (Matrix nrs ncs rs) (Matrix nrs' ncs' rs')
  | nrs /= nrs' = error "Matrix.blockSumRows: unequal number of rows"
  | otherwise   = Matrix nrs (ncs+ncs') $
                    V.zipWith (<>) rs rs'

blockSumCols :: Matrix a -> Matrix a -> Matrix a
blockSumCols (Matrix nrs ncs rs) (Matrix nrs' ncs' rs')
  | ncs /= ncs' = error "Matrix.blockSumCols: unequal number of cols"
  | otherwise   = Matrix (nrs+nrs') ncs $ rs <> rs'

blockMatrix :: Vector (Vector (Matrix a)) -> Matrix a
blockMatrix = V.foldl1' blockSumCols . fmap (V.foldl1' blockSumRows)

blockMatrixL :: [[Matrix a]] -> Matrix a
blockMatrixL = blockMatrix . fmap V.fromList . V.fromList 

instance AdditiveMonoid a => Monoid (Matrix a) where
  mempty = M.zero 0 0
  mappend = blockSum

--------------------------------------------------------------------------------
-- concatMap
--------------------------------------------------------------------------------

concatMap :: (a -> Matrix b) -> Matrix a -> Matrix b
concatMap f (Matrix nrs ncs rs) = blockMatrix $ fmap (fmap f) rs

--------------------------------------------------------------------------------
-- submatrices
--------------------------------------------------------------------------------

headRows :: Matrix a -> Vector a
headRows (Matrix _ _ rs) = V.head rs

tailRows :: Matrix a -> Matrix a
tailRows (Matrix nrs ncs rs) = Matrix (pred nrs) ncs $ V.tail rs

headCols :: Matrix a -> Vector a
headCols (Matrix _ _ rs) = fmap V.head rs

tailCols :: Matrix a -> Matrix a
tailCols (Matrix nrs ncs rs) = Matrix nrs (pred ncs) $ fmap V.tail rs


splitAtRows :: Int -> Matrix a -> (Matrix a, Matrix a)
splitAtRows ix m@(Matrix nrs ncs rs)
  | ix < 0    = (zeroM, m)
  | ix >= ncs = (m, zeroM)
  | otherwise = (Matrix nrs ix rsLeft, Matrix nrs (ncs-ix) rsRight)
  where
   zeroM = Matrix nrs 0 $ V.replicate nrs V.empty
   (rsLeft,rsRight) = V.unzip $ fmap (V.splitAt ix) rs

splitAtCols :: Int -> Matrix a -> (Matrix a, Matrix a)
splitAtCols ix m@(Matrix nrs ncs rs)
  | ix < 0    = (zeroM, m)
  | ix >= nrs = (m, zeroM)
  | otherwise = (Matrix ix ncs rsTop, Matrix (nrs-ix) ncs rsBottom)
 where
   zeroM = Matrix 0 ncs V.empty
   (rsTop,rsBottom) = V.splitAt ix rs

sliceRows :: Int -> Int -> Matrix a -> Matrix a
sliceRows ix sz m@(Matrix nrs ncs rs)
  | ix < 0              = sliceRows 0 (sz+ix) m
  | ix >= nrs || sz < 0 = Matrix 0 ncs V.empty
  | ix+sz > nrs         = sliceRows ix (nrs-ix) m
  | otherwise           = Matrix sz ncs $ V.slice ix sz rs

sliceCols :: Int -> Int -> Matrix a -> Matrix a
sliceCols jx sz m@(Matrix nrs ncs rs)
  | jx < 0              = sliceCols 0 (sz+jx) m
  | jx >= ncs || sz < 0 = Matrix nrs 0 $ V.replicate ncs V.empty
  | jx+sz > ncs         = sliceCols jx (ncs-jx) m
  | otherwise           = Matrix nrs sz $ fmap (V.slice jx sz) rs
