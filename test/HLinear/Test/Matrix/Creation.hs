module HLinear.Test.Matrix.Creation
where

import Prelude hiding ( (*) )
import Control.Applicative ( (<$>), (<*>), (<|>) )
import Data.Composition ( (.:) )
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector ( Vector )
import Math.Structure
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import Test.Vector
import Test.Tasty

import HLinear.Test.Utils

import HLinear.Matrix


properties :: TestTree
properties = testGroup "Creation properties"
  [ testProperty "nmbRows/Cols .: zeroMatrix" $
      \nrs ncs -> ( let m = zeroMatrix nrs ncs :: Matrix Int
                    in    nmbRows m == nrs
                       && nmbCols m == ncs )

  , testProperty "nmbRows/Cols .: identityMatrix" $
      \nrs -> ( let m = identityMatrix nrs :: Matrix Int
                in    nmbRows m == nrs
                   && nmbCols m == nrs )

  , testPropertyMatrix "diagonalMatrix . * == * . diagonalMatrix" $
      \abs -> ( let (as,bs) = V.unzip (abs :: Vector (Int,Int))
                  in (diagonalMatrix as) * (diagonalMatrix bs) ==
                     ( diagonalMatrix (V.map (uncurry (*)) abs) ) )
  ]
