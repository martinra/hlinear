module HLinear.Matrix.Definition
where

import HLinear.Utility.Prelude


-- | Matrix a models the space of all matrices
-- with partially defined addition and
-- multiplication. It is only a semiring, since
-- even zero cannot be defined for all the spaces
-- simultaneously.
data Matrix a =
  Matrix
    !Int                 -- number of rows
    !Int                 -- number of columns
    !(Vector (Vector a)) -- rows from top to bottom

class IsMatrix a b where
  toMatrix :: a -> Matrix b
