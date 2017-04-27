module HLinear.Test.Matrix.Basic
where

import Prelude hiding ( (+) )
import Control.Applicative ( (<$>), (<*>), (<|>) )
import Data.Maybe
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural ()
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty

import HLinear.Test.Utility.Misc
import HLinear.Matrix as M


properties :: TestTree
properties = testGroup "Basic properties"
  [ testPropertyMatrix "m == m" $
      \m -> (m :: Matrix Int) == m

  , testProperty "isZero (zero * v)"
      \v -> isZero $ zero :: MatrixSized 4 3 * (v :: MatrixSized 3 1)

  , testProperty "nmbRows/Cols .: zeroMatrix" $
      \nrs ncs -> ( let m = M.zero nrs ncs :: Matrix Int
                    in    nmbRows m == nrs
                       && nmbCols m == ncs )

  , testProperty "nmbRows/Cols .: identityMatrix" $
      \nrs -> ( let m = M.one nrs :: Matrix Int
                in    nmbRows m == nrs
                   && nmbCols m == nrs )

  , testPropertyMatrix "diagonalMatrix . * == * . diagonalMatrix" $
      \abs -> ( let (as,bs) = V.unzip (abs :: Vector (Int,Int))
                in  (diagonal as) * (diagonal bs) ==
                      diagonal (V.map (uncurry (*)) abs)
              )
  ]
