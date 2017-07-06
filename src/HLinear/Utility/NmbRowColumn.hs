module HLinear.Utility.NmbRowColumn
where

import Prelude

class HasNmbRows a where
  nmbRows :: a -> Int
  nmbRows = nmbRows'

  nmbRows' :: Integral b => a -> b
  nmbRows' = fromIntegral . nmbRows

class HasNmbCols a where
  nmbCols :: a -> Int
  nmbCols = nmbCols'

  nmbCols' :: Integral b => a -> b
  nmbCols' = fromIntegral . nmbCols
