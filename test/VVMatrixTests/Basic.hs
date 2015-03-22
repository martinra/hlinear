module VVMatrixTests.Basic
where

import Control.Applicative ( (<$>), (<*>), (<|>) )
import Math.Structure
import Data.Maybe
import qualified Data.Vector as V
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty

import TestHLinear.Utils

import HLinear.VVMatrix


properties :: TestTree
properties = testGroup "Basic properties"
  [ testProperty "m == m" $ \m -> (m :: VVMatrix Int) == m
  , testProperty "m == forceVV m" $
      \m -> (m :: VVMatrix Int) == fromMaybe m (forceVVMay m)
  , testProperty "m == forceSize m" $
      \m nrs ncs -> (m :: VVMatrix Int) ==
                    ( let nrs' = fromMaybe nrs $ nmbRows m
                          ncs' = fromMaybe ncs $ nmbCols m
                      in forceSize nrs' ncs' m )
  ]
