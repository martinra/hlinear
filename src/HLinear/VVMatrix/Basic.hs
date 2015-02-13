import qualified Data.Vector as V

toLists :: VVMatrix a -> [[a]]
toLists (VVMatrix _ _ vss) = V.toList $ V.map V.toList vss

fromLists :: [[a]] -> VVMatrix a
fromLists rs | nrs == 0  = VVMatrix 0 0 []
             | otherwise = if any ((\=ncs) . length) $ tail rs
                           then error "rows must have the same length"
                           else VVMatrix nrs ncs $ V.toList (map V.toList rs)
  where
    nrs = length nrs
    ncs = length $ head rs
