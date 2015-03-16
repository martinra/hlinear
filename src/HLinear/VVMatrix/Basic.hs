module HLinear.VVMatrix.Basic
where

import qualified Data.Vector as V
import Data.Vector ( Vector )
import HLinear.VVMatrix.Definition ( VVMatrix(..) )


nrows :: VVMatrix a -> Int
nrows (VVMatrix nrs _ _) = nrs

ncols :: VVMatrix a -> Int
ncols (VVMatrix _ ncs _) = ncs


toVectors :: VVMatrix a -> Vector (Vector a)
toVectors (VVMatrix _ _ rs) = rs

fromVectors :: Vector (Vector a) -> VVMatrix a
fromVectors rs =
  fromVectors' nrs (if nrs == 0 then 0 else V.length (V.head rs)) rs
  where
  nrs = V.length rs

fromVectors' :: Int -> Int -> Vector (Vector a) -> VVMatrix a
fromVectors' nrs ncs rs
  | nrs /= V.length rs = error "number of rows incorrect"
  | any ((/=ncs) . V.length) rs = error "rows must have the same length"
  | otherwise =  VVMatrix nrs ncs rs


toLists :: VVMatrix a -> [[a]]
toLists (VVMatrix _ _ rs) = V.toList $ V.map V.toList rs

fromLists :: [[a]] -> VVMatrix a
fromLists rs = fromVectors $ V.map V.fromList $ V.fromList rs

fromLists' :: Int -> Int -> [[a]] -> VVMatrix a
fromLists' nrs ncs rs = fromVectors' nrs ncs $ V.map V.fromList $ V.fromList rs


instance Eq a => Eq (VVMatrix a) where
  (VVMatrix nrs ncs rs) == (VVMatrix nrs' ncs' rs') =
    nrs == nrs' && ncs == ncs' && rs == rs'

instance Show a => Show (VVMatrix a) where
  show (VVMatrix 0 _ rs) = "[ ]"
  show (VVMatrix _ 0 rs) = "[ ]"
  show (VVMatrix _ _ rs) =
    V.foldl1 (\r r' -> r ++ "\n" ++ r') $ V.map show' rsShown
    where
    rsShown = V.map (V.map show) rs
    show' r= "[ " ++ rShown ++ " ]"
      where
      rShown = V.foldl1 (\a a' -> a ++ " " ++ a') $ V.map center r
    center s = replicate n ' ' ++ s ++ replicate n' ' '
      where
      maxLength = V.maximum $ V.map (V.maximum . V.map length) rsShown
      n = (maxLength - length s) `div` 2
      n' = maxLength - n - length s
