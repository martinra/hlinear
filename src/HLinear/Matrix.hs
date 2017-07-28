module HLinear.Matrix
  ( Matrix(..)
  , IsMatrix(..)
  , IsMatrixFactorization(..)
  , MatrixInvertible

  , (!)
  , (!?)

  , fromVectors
  , fromVectors'

  , fromLists
  , fromLists'

  , fromColumns
  , fromColumns'

  , fromVectorsSafe
  , fromVectorsSafe'

  , fromListsSafe
  , fromListsSafe'

  , fromColumnsSafe
  , fromColumnsSafe'

  , toVectors
  , toLists
  , toColumns

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
  , elementary
  , tensorProduct
  , transpose
  , diagonal

  , blockSum
  , blockSumRows
  , blockSumCols
  , blockMatrix
  , blockMatrixL

  , Column(..)
  )
where

import qualified Prelude

import HLinear.Matrix.Algebra
import HLinear.Matrix.Basic
import HLinear.Matrix.Block
import HLinear.Matrix.Column ( Column(..) )
import HLinear.Matrix.Definition
import HLinear.Matrix.Fraction
import HLinear.Matrix.Invertible

import HLinear.Matrix.QuickCheck ()
import HLinear.Matrix.SmallCheck ()
