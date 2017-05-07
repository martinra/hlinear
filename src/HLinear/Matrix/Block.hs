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
    zeros = V.replicate (fromIntegral ncs) zero 
    zeros' = V.replicate (fromIntegral ncs') zero 

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
  | ix < 0     = (zeroM, m)
  | ix >= ncsZ = (m, zeroM)
  | otherwise  = (Matrix nrs ncsLeft rsLeft, Matrix nrs ncsRight rsRight)
  where
   nrsZ = fromIntegral nrs
   ncsZ = fromIntegral ncs

   zeroM = Matrix nrs 0 $ V.replicate nrsZ V.empty

   ncsLeft = fromIntegral ix
   ncsRight = fromIntegral $ ncsZ - ix
   (rsLeft,rsRight) = V.unzip $ fmap (V.splitAt ix) rs

splitAtCols :: Int -> Matrix a -> (Matrix a, Matrix a)
splitAtCols ix m@(Matrix nrs ncs rs)
  | ix < 0     = (zeroM, m)
  | ix >= nrsZ = (m, zeroM)
  | otherwise  = (Matrix nrsTop ncs rsTop, Matrix nrsBottom ncs rsBottom)
 where
   nrsZ = fromIntegral nrs

   zeroM = Matrix 0 ncs V.empty

   nrsTop = fromIntegral ix
   nrsBottom = fromIntegral $ nrsZ - ix
   (rsTop,rsBottom) = V.splitAt ix rs

sliceRows :: Int -> Int -> Matrix a -> Matrix a
sliceRows ix sz m@(Matrix nrs ncs rs)
  | ix < 0 = sliceRows 0 (sz+ix) m
  | ix >= nrsZ || sz < 0 = Matrix 0 ncs V.empty
  | ix+sz > nrsZ = sliceRows ix (nrsZ-ix) m
  | otherwise = Matrix szN ncs $ V.slice ix sz rs
  where
   nrsZ = fromIntegral nrs
   szN = fromIntegral sz

sliceCols :: Int -> Int -> Matrix a -> Matrix a
sliceCols jx sz m@(Matrix nrs ncs rs)
  | jx < 0 = sliceCols 0 (sz+jx) m
  | jx >= ncsZ || sz < 0 = Matrix nrs 0 $ V.replicate ncsZ V.empty
  | jx+sz > ncsZ = sliceCols jx (ncsZ-jx) m
  | otherwise = Matrix nrs szN $ fmap (V.slice jx sz) rs
  where
   ncsZ = fromIntegral ncs
   szN = fromIntegral sz
