module HLinear.Hook.EchelonTransformation.Definition
where

import Data.Vector ( Vector(..) )
import Numeric.Natural ( Natural )

import HLinear.Hook.EchelonTransformation.Column


-- | An echelon transformation preserves Echelon forms, and thus is of the form
--   1  v  v  v
--   0  1  v  v
--   0  0  1  v
--   0  0  0  1
--  EchelonTransformationColumns all start at the top, but their lengths vary,
--  which is expressed by their offset parameter.
data EchelonTransformation a =
  EchelonTransformation
    { _nmbRows :: Natural
    , columns :: Vector (EchelonTransformationColumn a)
    }

