module VVMatrixTests.Creation
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
properties = testGroup "Creation properties"
  [ testProperty "fromVectors' . toVectors == id" $ \m ->
      ( let nrs = nmbRows m
            ncs = nmbCols m
            rs  = toVectors m
        in fromMaybe
           ( if m == zero
             then m == zeroMatrix' nrs ncs
             else m == oneMatrix' nrs (forceSize 1 1 m ! 0 V.! 0) )
           ( (==) (m :: VVMatrix Int) <$>
             ( fromVectors' <$> nrs <*> ncs <*> rs ) )
      )
  ]
