module HLinear.VVMatrix.Basic
where

import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import HLinear.VVMatrix.Definition (VVMatrix(..))

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

instance Eq (VVMatrix a) where
  (==) = VG.eq
