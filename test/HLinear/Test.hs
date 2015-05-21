module Main
where

import Test.Tasty ( defaultMain
                  , testGroup
                  )

import HLinear.Test.PLE ( pleTests )
import HLinear.Test.VVMatrix ( vvMatrixTests )


main = defaultMain $
  testGroup "HLinear Tests"
  [ vvMatrixTests
  , pleTests
  ]
