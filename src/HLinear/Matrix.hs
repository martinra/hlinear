module HLinear.Matrix
  ( Matrix(..)

  , toBRMatrix
  , fromBRMatrix

  , (!)
  , (!?)

  , fromVectors
  , fromVectors'

  , fromVectorsUnsafe
  , fromVectorsUnsafe'

  , fromLists
  , fromLists'

  , fromListsUnsafe
  , fromListsUnsafe'

  , headRows
  , tailRows
  , headCols
  , tailCols

  , zeroMatrix
  , identityMatrix
  , diagonalMatrix

  , blockSumRows
  , blockSumCols
  , blockMatrix

  , Column(Column)
  , unColumn
  )
where

import HLinear.Matrix.Algebra
import HLinear.Matrix.Basic
import HLinear.Matrix.Conversion
import HLinear.Matrix.Definition
import HLinear.Matrix.StdElements

import HLinear.Matrix.QuickCheck
import HLinear.Matrix.SmallCheck
