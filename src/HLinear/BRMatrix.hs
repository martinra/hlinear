module HLinear.BRMatrix
  ( BRMatrix

  , nmbRows
  , nmbCols

  , (!)

  , fromVectors
  , fromVectors'

  , fromVectorsUnsafe
  , fromVectorsUnsafe'

  , fromLists
  , fromLists'

  , fromListsUnsafe
  , fromListsUnsafe'

  , map

  , zeroMatrix
  , identityMatrix
  , diagonalMatrix

  , Column(Column)
  , unColumn
  )

where

import qualified Prelude

import HLinear.BRMatrix.Algebra
import HLinear.BRMatrix.Basic
import HLinear.BRMatrix.Definition
import HLinear.BRMatrix.RVector hiding ( (!) )
import HLinear.BRMatrix.StdElements

import HLinear.BRMatrix.QuickCheck
import HLinear.BRMatrix.SmallCheck
