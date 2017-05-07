module HLinear.Hook.EchelonForm.Definition
where

import HLinear.Utility.Prelude

import HLinear.Hook.EchelonForm.Row


-- | represents a matrix of given size, efficient for echelon forms
--   The offset is assumed to be strictly increasing
--   Example: the vector with entries Rows (1, vs) and (3, vs)
--   corresponds to
--     0 v v v v ...
--     0 0 0 v v ...

data EchelonForm a =
  EchelonForm
    { _nmbRows :: Natural
    , _nmbCols :: Natural
    , rows :: Vector (EchelonFormRow a)
    }
