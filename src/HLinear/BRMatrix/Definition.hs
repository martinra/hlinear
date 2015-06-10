module HLinear.BRMatrix.Definition
where

import Data.Vector ( Vector )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.BRMatrix.RVector ( RVector(RVector) )
import qualified HLinear.BRMatrix.RVector as RV


-- | BRMatrix a, as a type, models the rng
--    \liminv Mat_{n,m}(a),
--   where the inverse limit is taken with respect to embeddings
--     Mat_{n,m}(a) \hookrightarrow Mat_{n+n',m+m'}(a)
--   that maps a matrix to the bottom right (BR) of the target space.
data BRMatrix a =
  BRMatrix { nmbRows :: !Natural
           , nmbCols :: !Natural
           , rows    :: RVector (RVector a)
           }

minimizeSize :: DecidableZero a => BRMatrix a -> BRMatrix a
minimizeSize m = BRMatrix nrs ncs (RVector rs)
  where
    rs = V.map RV.minimizeSize $
         V.dropWhile (V.all isZero . RV.toCurrentVector) $
         RV.toCurrentVector $ rows m
    nrs = fromIntegral $ V.length rs
    ncs = V.maximum $ V.map RV.currentLength rs

-- row access

(!) :: BRMatrix a -> Int -> RVector a
(!) = (V.!) . RV.toCurrentVector . rows

(!?) :: BRMatrix a -> Int -> Maybe (RVector a)
(!?) = (V.!?) . RV.toCurrentVector . rows
