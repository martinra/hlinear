module HLinear.PLE.Hook.EchelonTransformation.QuickCheck
where

import Control.Arrow ( (&&&) )
import Data.Traversable ( forM )
import qualified Data.Vector as V
import Data.Vector ( Vector )
import qualified Data.Vector.Mutable as VM
import Math.Structure ( DecidableZero, NonZero )

import Math.Structure.Tasty ()
import Test.QuickCheck ( suchThat, Gen )
import Test.QuickCheck.Arbitrary ( Arbitrary
                                 , arbitrary
                                 , shrink
                                 )
import Test.QuickCheck.Modifiers ( NonNegative(..)
                                 , Small(..)
                                 )

import HLinear.PLE.Hook.EchelonTransformation.Basic as LT
import HLinear.PLE.Hook.EchelonTransformation.Column as LTC


instance    (DecidableZero a, Arbitrary a)
         => Arbitrary (EchelonTransformation a) where
  arbitrary = do
    -- We use this slightly odd construction of nrs to avoid infinite loops
    -- that QuickCheck sometimes produces on using
    --   ncs <- arbitary `suchThat` (<nrs)
    NonNegative ncs <- arbitrary
    NonNegative nrsDiff <- arbitrary
    let nrs = ncs + nrsDiff

    cs <- V.generateM ncs $ \jx -> do
            c <- V.replicateM (nrs-jx-1) arbitrary
            return $ EchelonTransformationColumn jx c
              
    return $ EchelonTransformation (fromIntegral nrs) cs

  shrink lt@(EchelonTransformation nrs cs)
    | nrs <= 1 || V.length cs <= 1 = []
    | otherwise =
        ltLeft:ltRight:
        [ EchelonTransformation nrs $ V.update cs $ V.singleton (jx,c)
        | jx <- [0..ncs-1]
        , c <- shrinkColumn $ cs V.! jx
        ]
        where
          ncs = V.length cs
          (ltLeft,ltRight) = LT.splitAt (fromIntegral $ ncs `div` 2) lt

          shrinkColumn (EchelonTransformationColumn s c) =
            [ EchelonTransformationColumn s $ V.update c $ V.singleton (ix,e)
            | ix <- [0..V.length c - 1]
            , e <- shrink (c V.! ix)
            ]
