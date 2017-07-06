module HLinear.Test.Matrix.Basic
where

import HLinear.Utility.Prelude

import Control.Applicative ( (<$>), (<*>), (<|>) )
import Math.Structure.Tasty
import Test.Tasty
import qualified Data.Vector as V

import HLinear.Utility.NmbRowColumn
import HLinear.Matrix ( Matrix )
import qualified HLinear.Matrix as M
import HLinear.Matrix.Sized ( MatrixSized )


properties :: TestTree
properties = testGroup "Basic properties"
  [ testPropertyQSnC 2 "m == m" $
      \m -> (m :: Matrix Int) == m

  , testPropertyQSC "isZero (zero * v)" $
      \v -> isZero $ (zero :: MatrixSized 3 3 FMPZ) *. (v :: MatrixSized 3 1 FMPZ)

  , testPropertyQSC "nmbRows/Cols .: zeroMatrix" $
      \nrs ncs -> ( let m = M.zero nrs ncs :: Matrix Int
                    in    nmbRows m == nrs
                       && nmbCols m == ncs )

  , testPropertyQSC "nmbRows/Cols .: identityMatrix" $
      \nrs -> ( let m = M.one nrs :: Matrix Int
                in    nmbRows m == nrs
                   && nmbCols m == nrs )

  , testPropertyQSnC 2 "diagonalMatrix . * == * . diagonalMatrix" $
      \abs -> ( let (as,bs) = V.unzip (abs :: Vector (Int,Int))
                in  (M.diagonal as) * (M.diagonal bs) ==
                      M.diagonal (V.map (uncurry (*)) abs)
              )
  ]
