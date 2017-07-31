module HLinear.Hook.EchelonTransformation.Definition
where

import HLinear.Utility.Prelude

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
    !Int                                      -- number of rows
    !(Vector (EchelonTransformationColumn a)) -- columns from right to left
