module HLinear.VVMatrix.Basic
where

import qualified Data.Vector as V
import HLinear.VVMatrix.Definition ( VVMatrix(..) )

toLists :: VVMatrix a -> [[a]]
toLists (VVMatrix _ _ vss) = V.toList $ V.map V.toList vss

fromLists :: [[a]] -> VVMatrix a
fromLists rs | nrs == 0  = VVMatrix 0 0 V.empty
             | otherwise = if any ((/=ncs) . length) $ tail rs
                           then error "rows must have the same length"
                           else VVMatrix nrs ncs $ V.fromList (map V.fromList rs)
  where
    nrs = length rs
    ncs = length $ head rs

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
