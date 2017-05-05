{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module HLinear.Matrix.Definition
where

import Data.Vector ( Vector )
import Numeric.Natural ( Natural )


-- | Matrix a models the space of all matrices
-- with partially defined addition and
-- multiplication. It is only a semiring, since
-- even zero cannot be defined for all the spaces
-- simultaneously.
data Matrix a =
  Matrix { _nmbRows :: !Natural
         , _nmbCols  :: !Natural
         , _rows :: Vector (Vector a)
         }

class IsMatrix a b where
  toMatrix :: a -> Matrix b
