module HLinear.PLE.Hook.LeftTransformation.Definition
where

import Data.Vector
import Numeric.Natural

import HLinear.Matrix ( Matrix )
import HLinear.PLE.Hook.LeftTransformation.Column


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
      { nmbRows :: Natural
      , columns :: Vector (LeftTransformationColumn a)
      }
  | LeftTransformationMatrix (Matrix a)
