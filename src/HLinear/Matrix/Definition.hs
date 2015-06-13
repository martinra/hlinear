module HLinear.Matrix.Definition
where

import Data.Vector ( Vector )
import qualified Data.Vector as V
import Numeric.Natural ( Natural )


-- | Matrix a models the space of all matrices
-- with partially defined addition and
-- multiplication. It is only a semiring, since
-- even zero cannot be defined for all the spaces
-- simultaneously.
data Matrix a =
  Matrix { nmbRows :: !Natural
         , nmbCols :: !Natural
         , rows    :: Vector (Vector a)
         }
