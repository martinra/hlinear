{-# LANGUAGE
    ScopedTypeVariables
  #-}

module HLinear.BRMatrix.QuickCheck
where

import Math.Structure

import Test.QuickCheck.Arbitrary ( Arbitrary
                                 , arbitrary
                                 , shrink
                                 )

import HLinear.BRMatrix.Definition ( BRMatrix(..) )
import HLinear.Matrix.Conversion
import HLinear.Matrix.QuickCheck


instance ( Arbitrary a, DecidableZero a ) => Arbitrary (BRMatrix a) where
  arbitrary = toBRMatrix <$> arbitrary
  shrink = map toBRMatrix . shrink . fromBRMatrix
