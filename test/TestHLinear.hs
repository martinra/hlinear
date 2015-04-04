module Main
where

import Test.Tasty ( defaultMain
                  , testGroup
                  )

import PLETests ( pleTests )
import VVMatrixTests ( vvMatrixTests )


main = defaultMain $
  testGroup "HLinear Tests"
  [ vvMatrixTests
  , pleTests
  ]
