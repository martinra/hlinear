module HLinear.PLE.Hook.EchelonForm.Definition
where

import Data.Vector ( Vector )
import Numeric.Natural ( Natural )

import HLinear.PLE.Hook.EchelonForm.Row


-- | represents a matrix of given size, efficient for echelon forms
--   The offset is assumed to be strictly increasing
--   Example: the vector with entries Rows (1, vs) and (3, vs)
--   corresponds to
--     0 v v v v ...
--     0 0 0 v v ...
data EchelonForm a =
  EchelonForm
    { nmbRows :: Natural
    , nmbCols :: Natural
    , rows :: Vector (EchelonFormRow a)
    }
