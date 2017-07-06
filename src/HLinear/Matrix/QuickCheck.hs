{-# LANGUAGE
    ScopedTypeVariables
  #-}

module HLinear.Matrix.QuickCheck
where

import qualified Prelude as P
import HLinear.Utility.Prelude

import Test.QuickCheck.Arbitrary ( Arbitrary, arbitrary, shrink )
import Test.QuickCheck.Modifiers ( NonNegative(..), Small(..) )
import qualified Data.Vector as V

import HLinear.Matrix.Definition ( Matrix(..) )


instance Arbitrary a => Arbitrary (Matrix a) where
  arbitrary = do
    NonNegative (Small nrs) <- arbitrary
    NonNegative (Small ncs) <- arbitrary
    rs <- V.replicateM nrs $
          V.replicateM ncs arbitrary
    return $ Matrix nrs ncs rs

  shrink (Matrix nrs ncs rs)
    | nrs <= 1 || ncs <= 1 = []
    | otherwise =
      fmap ($rs)
      [ Matrix nrs ncsD2 . fmap (V.take ncsD2)
      , Matrix nrs ncsR2 . fmap (V.drop ncsD2)
      , Matrix nrsD2 ncs . V.take nrsD2
      , Matrix nrsR2 ncs . V.drop nrsD2
      ]
      <>
      [ Matrix nrs ncs $
          V.update rs $ V.singleton
            ( ix, V.update (rs V.! ix) $ V.singleton (jx, e) )
      | ix <- [0..nrs-1]
      , jx <- [0..ncs-1]
      , e  <- shrink $ rs V.! ix V.! jx
      ]
      where
        nrsD2 = nrs `P.div` 2
        nrsR2 = nrs - nrsD2
        ncsD2 = ncs `P.div` 2
        ncsR2 = ncs - ncsD2
