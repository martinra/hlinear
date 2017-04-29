module HLinear.Utility.NmbRowColumn
where

import Numeric.Natural ( Natural )


class HasNmbRows a where
  nmbRows :: a -> Natural
  nmbRows = nmbRows'

  nmbRows' :: Integral b => a -> b
  nmbRows' = fromIntegral . nmbRows

class HasNmbCols a where
  nmbCols :: a -> Natural
  nmbCols = nmbCols'

  nmbCols' :: Integral b => a -> b
  nmbCols' = fromIntegral . nmbCols
