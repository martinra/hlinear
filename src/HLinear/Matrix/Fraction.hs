{-# LANGUAGE UndecidableInstances #-}

module HLinear.Matrix.Fraction
where

import HLinear.Utility.Prelude

import qualified Data.Vector as V

import HLinear.Utility.Fraction
import HLinear.Matrix.Definition
import qualified HLinear.Matrix.Basic as M


instance
     ( IsFraction a n (NonZero d)
     , Ring a, EuclideanDomain d, MultiplicativeSemigroupRightAction d n )
  => IsFraction (Matrix a) (Matrix n) (Vector (NonZero d))
  where
  {-# INLINABLE toFraction #-}
  toFraction (Matrix nrs ncs rs) =
    let (ns,ds) = V.unzip $ fmap (\r -> let Fraction n d = toFraction r in (n,d)) rs
    in  Fraction (Matrix nrs ncs ns) ds
  {-# INLINABLE fromFraction #-}
  fromFraction (Fraction (Matrix nrs ncs rs) ds) =
    Matrix nrs ncs $ V.zipWith (\r d -> fromFraction $ Fraction r d) rs ds
  {-# INLINABLE fromNumerator #-}
  fromNumerator = fmap fromNumerator
  {-# INLINABLE fromDenominator #-}
  fromDenominator = M.diagonal . fmap fromDenominator
