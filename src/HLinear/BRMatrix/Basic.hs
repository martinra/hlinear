module HLinear.BRMatrix.Basic
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.DeepSeq ( NFData, rnf )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural

import HLinear.BRMatrix.Definition
import HLinear.BRMatrix.RVector ( RVector(RVector) )
import qualified HLinear.BRMatrix.RVector as RV


-- this is equality as elements of the inverse limit, that is disregarding
-- zeros at the left and the top
instance   ( Eq a, DecidableZero a ) =>  Eq (BRMatrix a) where
  BRMatrix _ _ rs == BRMatrix _ _ rs' = rs == rs'

instance Show a => Show (BRMatrix a) where
  show (BRMatrix 0 ncs rs) = "[ BRMatrix 0 " ++ show ncs ++ " ]"
  show (BRMatrix nrs 0 rs) = "[ BRMatrix " ++ show nrs ++ " 0 ]"
  show (BRMatrix _ _ rs) = 
    V.foldl1 (\r r' -> r ++ "\n" ++ r') $ V.map show' shownEntries
      where
      shownEntries = V.map (V.map show . RV.toCurrentVector) $
                       RV.toCurrentVector rs
      maxLength = V.maximum $ V.map (V.maximum . V.map length) shownEntries
      show' r= "[ " ++ rShown ++ " ]"
        where
        rShown = V.foldl1 (\a a' -> a ++ " " ++ a') $ V.map center r
      center s = replicate n ' ' ++ s ++ replicate n' ' '
        where
        n = (maxLength - length s) `div` 2
        n' = maxLength - n - length s

instance NFData a => NFData (BRMatrix a) where
  rnf (BRMatrix nrs ncs rs) =
    rnf nrs `seq` rnf ncs `seq` rnf rs `seq` ()

-- construction of matrices from vectors or lists

fromVectors :: Vector (Vector a) -> Either String (BRMatrix a)
fromVectors rs =
  -- todo: introduce unsafe version of fromVectors'
  fromVectors' nrs ncs rs
    where
    nrs = fromIntegral $ V.length rs
    ncs = if nrs == 0 then 0 else fromIntegral $ V.length (V.head rs)

fromVectors' :: Natural -> Natural
             -> Vector (Vector a) -> Either String (BRMatrix a)
fromVectors' nrs ncs rs
  | nrs /= fromIntegral (V.length rs) = Left
      "HLinear.BRMatrix fromVectors': incorrect number of rows"
  | any ((/=ncs) . fromIntegral . V.length) rs = Left
      "HLinear.BRMatrix fromVectors': rows must have the same length"
  | otherwise = Right $ BRMatrix nrs ncs $ RVector $ V.map RVector rs


fromLists :: [[a]] -> Either String (BRMatrix a)
fromLists = fromVectors . V.map V.fromList . V.fromList

fromLists' :: Natural -> Natural -> [[a]] -> Either String (BRMatrix a)
fromLists' nrs ncs = fromVectors' nrs ncs . V.map V.fromList . V.fromList
