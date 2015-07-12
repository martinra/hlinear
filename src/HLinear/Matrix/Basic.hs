module HLinear.Matrix.Basic
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.DeepSeq ( NFData, rnf )
import Data.Binary
import Data.Composition ( (.:), (.:.) )
import Data.Maybe
import qualified Data.Permute as P
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural

import HLinear.Matrix.Conversion
import HLinear.Matrix.Definition

instance Eq a => Eq (Matrix a) where
  (Matrix nrs ncs rs) == (Matrix nrs' ncs' rs') =
    nrs == nrs' && ncs == ncs' && rs == rs'

instance Show a => Show (Matrix a) where
  show (Matrix 0 ncs rs) = "[ Matrix 0 x " ++ show ncs ++ " ]"
  show (Matrix nrs 0 rs) = "[ Matrix " ++ show nrs ++ " x 0 ]"
  show (Matrix _ _ rs) = showMatrixAsRows rs

showMatrixAsRows :: Show a => Vector (Vector a) -> String
showMatrixAsRows rs =
  V.foldl1 (\r r' -> r ++ "\n" ++ r') $ V.map show' shownEntries
    where
    shownEntries = V.map (V.map show) rs
    maxLength = V.maximum $ V.map (V.maximum . V.map length) shownEntries
    show' r= "[ " ++ rShown ++ " ]"
      where
      rShown = V.foldl1 (\a a' -> a ++ " " ++ a') $ V.map center r
    center s = replicate n ' ' ++ s ++ replicate n' ' '
      where
      n = (maxLength - length s) `div` 2
      n' = maxLength - n - length s

instance NFData a => NFData (Matrix a) where
  rnf (Matrix nrs ncs rs) =
    seq (rnf nrs) $
    seq (rnf ncs) $
    seq (rnf rs) ()

instance Binary a => Binary (Matrix a) where
  put (Matrix nrs ncs rs) = do
    put nrs
    put ncs
    V.forM_ rs $ V.mapM_ put

  get = do
    nrs <- get
    ncs <- get
    rs <- V.replicateM (fromIntegral nrs) $ V.replicateM (fromIntegral ncs) get
    return $ Matrix nrs ncs rs

-- row access

(!) :: Matrix a -> Int -> Vector a
(!) = fromJust .: (!?)

(!?) :: Matrix a -> Int -> Maybe (Vector a)
(!?) = (V.!?) . rows

-- permutation of rows

permuteRows :: P.Permute -> Matrix a -> Matrix a
permuteRows p (Matrix nrs ncs rs)
  | np /= nrsZ = error $ "Matrix.permuteRow: permutation size " ++
                         "does not match number of rows"
  | otherwise  = Matrix nrs ncs $
                   V.backpermute rs $ V.generate np $ \ix -> p `P.at` ix
  where
    np = P.size p
    nrsZ = fromIntegral nrs

-- construction of matrices from vectors or lists

fromVectors :: Vector (Vector a) -> Either String (Matrix a)
fromVectors rs = 
  fromVectors' nrs ncs rs
    where
    nrs = fromIntegral $ V.length rs
    ncs = if nrs == 0 then 0 else fromIntegral $ V.length (V.head rs)

fromVectors' :: Natural -> Natural -> Vector (Vector a)
             -> Either String (Matrix a)
fromVectors' nrs ncs rs
  | nrs /= fromIntegral (V.length rs) = Left
      "HLinear.Matrix fromVectors': incorrect number of rows"
  | any ((/=ncs) . fromIntegral . V.length) rs = Left
      "HLinear.Matrix fromVectors': rows must have the same length"
  | otherwise = Right $ Matrix nrs ncs rs

fromLists :: [[a]] -> Either String (Matrix a)
fromLists = fromVectors . V.map V.fromList . V.fromList

fromLists' :: Natural -> Natural -> [[a]]
           -> Either String (Matrix a)
fromLists' nrs ncs = fromVectors' nrs ncs . V.map V.fromList . V.fromList

fromVectorsUnsafe = either undefined id . fromVectors
fromVectorsUnsafe' = either undefined id .:. fromVectors'

fromListsUnsafe = either undefined id . fromLists
fromListsUnsafe' = either undefined id .:. fromLists'

-- map

map :: (a -> b) -> Matrix a -> Matrix b
map f (Matrix nrs ncs rs) = Matrix nrs ncs $ V.map (V.map f) rs

-- submatrices

headRows :: Matrix a -> Vector a
headRows (Matrix _ _ rs) = V.head rs

tailRows :: Matrix a -> Matrix a
tailRows (Matrix nrs ncs rs) = Matrix (pred nrs) ncs $ V.tail rs

headCols :: Matrix a -> Vector a
headCols (Matrix _ _ rs) = V.map V.head rs

tailCols :: Matrix a -> Matrix a
tailCols (Matrix nrs ncs rs) = Matrix nrs (pred ncs) $ V.map V.tail rs


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
   (rsLeft,rsRight) = V.unzip $ V.map (V.splitAt ix) rs

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
