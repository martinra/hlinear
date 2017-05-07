module HLinear.Hook.EchelonTransformation.QuickCheck
where

import HLinear.Utility.Prelude
import qualified Prelude as P

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Math.Structure.Tasty ()
import Test.QuickCheck ( suchThat, Gen )
import Test.QuickCheck.Arbitrary ( Arbitrary, arbitrary, shrink )
import Test.QuickCheck.Modifiers ( NonNegative(..), Small(..) )

import HLinear.Hook.EchelonTransformation.Basic as ET
import HLinear.Hook.EchelonTransformation.Column as ETC
import HLinear.Hook.EchelonTransformation.Definition


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

  shrink et@(EchelonTransformation nrs cs)
    | nrs <= 1 || V.length cs <= 1 = []
    | otherwise =
        etLeft:etRight:
        [ EchelonTransformation nrs $ V.update cs $ V.singleton (jx,c)
        | jx <- [0..ncs-1]
        , c <- shrinkColumn $ cs V.! jx
        ]
        where
          nrsZ = fromIntegral nrs
          ncs = V.length cs
          (etLeft,etRight) = ET.splitAt (fromIntegral $ nrsZ - (ncs `P.div` 2)) et

          shrinkColumn (EchelonTransformationColumn s c) =
            [ EchelonTransformationColumn s $ V.update c $ V.singleton (ix,e)
            | ix <- [0..V.length c - 1]
            , e <- shrink (c V.! ix)
            ]
