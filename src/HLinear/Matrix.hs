module HLinear.Matrix
  ( Matrix(..)

  , toBRMatrix
  , fromBRMatrix

  , (!)
  , (!?)

  , permuteRows

  , fromVectors
  , fromVectors'

  , fromVectorsUnsafe
  , fromVectorsUnsafe'

  , fromLists
  , fromLists'

  , fromListsUnsafe
  , fromListsUnsafe'

  , map

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

import qualified Prelude

import HLinear.Matrix.Algebra
import HLinear.Matrix.Basic
import HLinear.Matrix.Conversion
import HLinear.Matrix.Definition
import HLinear.Matrix.StdElements

import HLinear.Matrix.QuickCheck
import HLinear.Matrix.SmallCheck
