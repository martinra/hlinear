module Main
where

import Test.Tasty ( defaultMain
                  , testGroup
                  )

import HLinear.Test.PLE ( pleTests )
import HLinear.Test.BRMatrix ( brMatrixTests )
import HLinear.Test.Matrix ( matrixTests )


main = defaultMain $
  testGroup "HLinear Tests"
  [-- matrixTests
   --, brMatrixTests
    pleTests
  ]
