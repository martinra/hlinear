module Main
where

import Test.Tasty ( defaultMain
                  , testGroup
                  )

import VVMatrixTests ( vvMatrixTestGroup )


main = defaultMain $
       testGroup "HLinear Tests" [vvMatrixTestGroup]
