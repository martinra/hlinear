{-# LANGUAGE
    ScopedTypeVariables
  #-}

module HLinear.Matrix.QuickCheck
where

import Control.Applicative ( (<$>) )
import qualified Data.Vector as V

import Test.QuickCheck.Arbitrary ( Arbitrary
                                 , arbitrary
                                 , shrink
                                 )
import Test.QuickCheck.Modifiers ( NonNegative(..)
                                 , Small(..)
                                 )

import HLinear.Matrix.Definition ( Matrix(..) )


instance Arbitrary a => Arbitrary (Matrix a) where
  arbitrary = do
    NonNegative (Small nrs) <- arbitrary
    NonNegative (Small ncs) <- arbitrary
    rs <- V.replicateM nrs $
          V.replicateM ncs arbitrary
    return $ Matrix (fromIntegral nrs) (fromIntegral ncs) rs

  shrink (Matrix nrs ncs rs)
    | nrs <= 1 || ncs <= 1 = []
    | otherwise =
      map ($rs)
      [ Matrix nrs ncsD2 . V.map (V.take $ fromIntegral ncsD2)
      , Matrix nrs ncsR2 . V.map (V.drop $ fromIntegral ncsD2)
      , Matrix nrsD2 ncs . V.take (fromIntegral nrsD2)
      , Matrix nrsR2 ncs . V.drop (fromIntegral nrsD2)
      ]
      ++
      [ Matrix nrs ncs $
          V.update rs $ V.singleton
            ( ix, V.update (rs V.! ix) $ V.singleton (jx, e) )
      | ix <- [0..fromIntegral nrs-1]
      , jx <- [0..fromIntegral ncs-1]
      , e  <- shrink $ rs V.! ix V.! jx
      ]
      where
        nrsD2 = nrs `div` 2
        nrsR2 = nrs - nrsD2
        ncsD2 = ncs `div` 2
        ncsR2 = ncs - ncsD2
