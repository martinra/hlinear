{-# LANGUAGE UndecidableInstances #-}

module HLinear.Hook.LeftTransformation.QuickCheck
where

import HLinear.Utility.Prelude
import qualified Prelude as P

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Math.Structure.Tasty ()
import Test.QuickCheck ( suchThat, Gen )
import Test.QuickCheck.Arbitrary ( Arbitrary, arbitrary, shrink )
import Test.QuickCheck.Modifiers ( NonNegative(..), Small(..) )

import HLinear.Utility.RPermute ( RPermute )
import HLinear.Hook.LeftTransformation.Algebra ()
import HLinear.Hook.LeftTransformation.Basic as LT
import HLinear.Hook.LeftTransformation.Column as LTC
import HLinear.Hook.LeftTransformation.Definition


instance ( Ring a, Arbitrary a, Arbitrary (Unit a) )
  => Arbitrary (LeftTransformation a)
  where
  arbitrary = do
    useLTColumn <- arbitrary
    if useLTColumn
    then ltFromColumns
    else do
      l <- ltFromColumns
      pl <- arbitrary :: Gen RPermute
      pr <- arbitrary :: Gen RPermute
      return $ (pl *. l) .* pr
    where
      ltFromColumns = do
        -- We use this slightly odd construction of nrs to avoid infinite loops
        -- that QuickCheck sometimes produces on using
        --   ncs <- arbitary `suchThat` (<nrs)
        NonNegative ncs <- arbitrary
        NonNegative nrsDiff <- arbitrary
        let nrs = ncs + nrsDiff
    
        cs <- V.generateM ncs $ \jx -> do
          a <- arbitrary
          c <- V.replicateM (nrs-jx-1) arbitrary
          return $ LeftTransformationColumn jx a c
                  
        return $ LeftTransformation nrs cs

  shrink lt@(LeftTransformation nrs cs)
    | nrs <= 1 || V.length cs <= 1 = []
    | otherwise =
        ltLeft:ltRight:
        [ LeftTransformation nrs $ V.update cs $ V.singleton (jx,c)
        | jx <- [0..ncs-1]
        , c <- shrinkColumn $ cs V.! jx
        ]
        where
          ncs = V.length cs
          (ltLeft,ltRight) = LT.splitAt (ncs `P.div` 2) lt

          shrinkColumn (LeftTransformationColumn s a c) =
            [ LeftTransformationColumn s a' c | a' <- shrink a ]
            <>
            [ LeftTransformationColumn s a $ V.update c $ V.singleton (ix,e)
            | ix <- [0..V.length c - 1]
            , e <- shrink (c V.! ix)
            ]

  -- todo: We would need to determine invertibility for shrinking matrices
  -- With decidable units that is possible.
  shrink (LeftTransformationMatrix _) = []
