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

import HLinear.Test.Utils
import HLinear.Matrix as M


properties :: TestTree
properties = testGroup "Basic properties"
  [ testProperty "zero matrix ! ix ! jx == 0" $
      \ix jx nrs ncs ->
        0 ==
        (M.zero (ix+1+nrs) (jx+1+ncs) :: Matrix Int)
        ! fromIntegral ix V.! fromIntegral jx

  , testProperty "zero matrix !? ix == Nothing" $
      \ix nrs ncs ->
        isNothing
        ( (M.zero nrs ncs :: Matrix Int) !? fromIntegral (nrs+ix) )

  , testPropertyMatrix "m == m" $
      \m -> (m :: Matrix Int) == m
  ]
