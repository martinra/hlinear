module HLinear.Hook.ERHook.Definition
where

import HLinear.Utility.Prelude

import HLinear.Hook.EchelonForm ( EchelonForm )
import HLinear.Hook.EchelonTransformation ( EchelonTransformation )
import HLinear.Matrix.Definition ( Matrix )


-- This models a transformation that acts on the vertical sum
-- of a matrix and an echelon form. In particular, we expect that
-- the second and third argument have the same number of columns.
data ERHook a =
  ERHook (EchelonTransformation a) (Matrix a) (EchelonForm a)
  deriving Show
