module HLinear.BRMatrix.Basic
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.DeepSeq ( NFData, rnf )
import Data.Composition ( (.:.), compose1, compose2, compose3 )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural

import qualified HLinear.Matrix.Basic as M
import qualified HLinear.Matrix.Conversion as M
import HLinear.BRMatrix.Definition
import HLinear.BRMatrix.RVector ( RVector(RVector) )
import qualified HLinear.BRMatrix.RVector as RV


-- this is equality as elements of the inverse limit, that is disregarding
-- zeros at the left and the top
instance   ( Eq a, DecidableZero a ) =>  Eq (BRMatrix a) where
  m == m' = toShortestVectors m == toShortestVectors m'

instance Show a => Show (BRMatrix a) where
  show (BRMatrix 0 ncs rs) = "[ BRMatrix 0 " ++ show ncs ++ " ]"
  show (BRMatrix nrs 0 rs) = "[ BRMatrix " ++ show nrs ++ " 0 ]"
  show (BRMatrix _ _ rs) = M.showMatrixAsRows $
                             V.map RV.toCurrentVector $ RV.toCurrentVector rs

instance NFData a => NFData (BRMatrix a) where
  rnf (BRMatrix nrs ncs rs) =
    rnf nrs `seq` rnf ncs `seq` rnf rs `seq` ()

-- row access

(!) :: BRMatrix a -> Int -> RVector a
(!) (BRMatrix nrs ncs rs) ix
  | ix < fromIntegral nrs = (V.! ix) $ RV.toCurrentVector rs
  | otherwise = RVector V.empty -- == zero

-- deconstruction of matrices

toShortestVectors :: DecidableZero a => BRMatrix a -> Vector (Vector a)
toShortestVectors (BRMatrix _ _ rs) =
  V.map RV.toShortestVector $ RV.toShortestVector rs

toShortestLists :: DecidableZero a => BRMatrix a -> [[a]]
toShortestLists = V.toList . V.map V.toList . toShortestVectors

-- construction of matrices from vectors or lists

fromVectors :: Vector (Vector a) -> Either String (BRMatrix a)
fromVectors = fmap M.toBRMatrix . M.fromVectors

fromVectors' :: Natural -> Natural -> Vector (Vector a)
             -> Either String (BRMatrix a)
fromVectors' = fmap M.toBRMatrix .:. M.fromVectors'

fromLists :: [[a]] -> Either String (BRMatrix a)
fromLists = fmap M.toBRMatrix . M.fromLists

fromLists' :: Natural -> Natural -> [[a]]
           -> Either String (BRMatrix a)
fromLists' = fmap M.toBRMatrix .:. M.fromLists'

fromVectorsUnsafe = M.toBRMatrix . M.fromVectorsUnsafe
fromVectorsUnsafe' = M.toBRMatrix .:. M.fromVectorsUnsafe'

fromListsUnsafe = M.toBRMatrix . M.fromListsUnsafe
fromListsUnsafe' = M.toBRMatrix .:. M.fromListsUnsafe'
