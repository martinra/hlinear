module HLinear.Test.BRMatrix.Basic
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

import HLinear.Test.Utils

import HLinear.BRMatrix
import qualified HLinear.BRMatrix.RVector as RV


properties :: TestTree
properties = testGroup "Basic properties"
  [ testProperty "zeroMatrix ! ix ! jx == 0" $
      \ix jx nrs ncs ->
        0 ==
        (zeroMatrix (ix+1+nrs) (jx+1+ncs) :: BRMatrix Int)
        ! fromIntegral ix RV.! fromIntegral jx

  , testPropertyMatrix "m == m" $
      \m -> (m :: BRMatrix Int) == m
  ]
