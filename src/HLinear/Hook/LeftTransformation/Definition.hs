module HLinear.Hook.LeftTransformation.Definition
where

import HLinear.Utility.Prelude

import HLinear.Matrix.Definition ( Matrix )
import HLinear.Hook.LeftTransformation.Column


 -- \ A vector of columns (a, [v]) which are offset by their index.
 --   It represents a transformation from the left
 --     a1     0     0   0
 --   v*a1    a2     0   0
 --   v*a1  v*a2    a3   0 
 --   v*a1  v*a2  v*a3  a4
 --   . . . .
 --
data LeftTransformation a =
    LeftTransformation
      Int                                   -- number of rows
      (Vector (LeftTransformationColumn a)) -- columns from left to right
  | LeftTransformationMatrix
      (Matrix a)                            -- invertible matrix
