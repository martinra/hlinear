module HLinear.VVMatrix.Basic
where

import qualified Data.Vector as V
import Data.Vector ( Vector )
import Math.Structure ( DecidableZero, isZero
                      , DecidableOne, isOne
                      )

import HLinear.VVMatrix.Definition ( VVMatrix(..) )


-- todo: change names nmbRows
nrows :: VVMatrix a -> Maybe Int
nrows (VVMatrix nrs _ _) = Just nrs
nrows _ = Nothing

ncols :: VVMatrix a -> Maybe Int
ncols (VVMatrix _ ncs _) = Just ncs
ncols _ = Nothing


toVectors :: VVMatrix a -> Maybe ( Vector (Vector a) )
toVectors (VVMatrix _ _ rs) = Just rs
toVectors _ = Nothing

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


toLists :: VVMatrix a -> Maybe [[a]]
toLists (VVMatrix _ _ rs) = Just $ V.toList $ V.map V.toList rs
toLists _ = Nothing

fromLists :: [[a]] -> VVMatrix a
fromLists rs = fromVectors $ V.map V.fromList $ V.fromList rs

fromLists' :: Int -> Int -> [[a]] -> VVMatrix a
fromLists' nrs ncs rs = fromVectors' nrs ncs $ V.map V.fromList $ V.fromList rs


instance   ( DecidableZero a, DecidableOne a, Eq a)
         =>  Eq (VVMatrix a) where
  (VVMatrix nrs ncs rs) == (VVMatrix nrs' ncs' rs') =
    nrs == nrs' && ncs == ncs' && rs == rs'
  Zero == Zero = True
  One == One = True
  Zero == One = False
  One == Zero = False
  Zero == (VVMatrix nrs ncs rs) = V.all (V.all isZero) rs
  One == (VVMatrix nrs ncs rs)
    | nrs /= ncs = False
    | otherwise = iall (\ix -> iall (\jx a -> if jx/=ix then isZero a else isOne a)) rs
    where
    iall :: (Int -> a -> Bool) -> Vector a -> Bool
    iall f v = V.ifoldr' (\ix a b -> b && f ix a) True v
  m == Zero = Zero == m
  m == One = One == m


instance Show a => Show (VVMatrix a) where
  show Zero = "ZeroMat"
  show One = "OneMat"
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
