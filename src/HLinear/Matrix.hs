module HLinear.Matrix
  ( Matrix(..)
  , IsMatrix(..)

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

  , zipWith

  , headRows
  , tailRows
  , headCols
  , tailCols

  , splitAtRows
  , splitAtCols
  , sliceRows
  , sliceCols

  , zero
  , isZero
  , one
  , isOne
  , diagonal

  , blockSum
  , blockSumRows
  , blockSumCols
  , blockMatrix
  , blockMatrixL

  , Column(Column)
  , unColumn
  )
where

import qualified Prelude

import HLinear.Matrix.Algebra
import HLinear.Matrix.Basic
import HLinear.Matrix.Definition
import HLinear.Matrix.Block

import HLinear.Matrix.QuickCheck
import HLinear.Matrix.SmallCheck
