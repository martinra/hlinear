module HLinear.BRMatrix
  ( BRMatrix

  , nmbRows
  , nmbCols

  , (!)
  , (!?)

  , fromVectors
  , fromVectors'

  , fromLists
  , fromLists'

  , zeroMatrix
  , identityMatrix
  , diagonalMatrix

  , Column
  , unColumn
  )

where

import HLinear.BRMatrix.Algebra
import HLinear.BRMatrix.Basic
import HLinear.BRMatrix.Definition
import HLinear.BRMatrix.RVector
import HLinear.BRMatrix.StdElements

import HLinear.BRMatrix.QuickCheck
import HLinear.BRMatrix.SmallCheck
