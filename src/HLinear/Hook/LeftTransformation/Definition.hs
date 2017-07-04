module HLinear.Hook.LeftTransformation.Definition
where

import Data.Vector
import Numeric.Natural

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
      Natural
      (Vector (LeftTransformationColumn a))
  | LeftTransformationMatrix
      (Matrix a)
