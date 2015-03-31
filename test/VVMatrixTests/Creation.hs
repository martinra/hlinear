module VVMatrixTests.Creation
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

  , testProperty "zero == zeroMatrix" $
      \nrs ncs -> zeroMatrix nrs ncs == (zero :: VVMatrix Int)
  , testProperty "nmbRows/Cols .: zeroMatrix" $
      \nrs ncs -> ( let m = zeroMatrix nrs ncs :: VVMatrix Int
                    in    nmbRows m == Just nrs
                       && nmbCols m == Just ncs )

  , testProperty "one == identityMatrix" $
      \nrs -> identityMatrix nrs == (one :: VVMatrix Int)
  , testProperty "nmbRows/Cols .: identityMatrix" $
      \nrs -> ( let m = identityMatrix nrs :: VVMatrix Int
                in    nmbRows m == Just nrs
                   && nmbCols m == Just nrs )

  , testProperty "diagonalMatrix . * = * . diagonalMatrix" $
      \abs -> ( let (as,bs) = V.unzip (abs :: Vector (Int,Int))
                  in (diagonalMatrix as) * (diagonalMatrix bs) ==
                     ( diagonalMatrix (V.map (uncurry (*)) abs) ) )
  ]
