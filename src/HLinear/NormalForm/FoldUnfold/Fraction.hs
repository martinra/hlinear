{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}


module HLinear.NormalForm.FoldUnfold.Fraction
where

import HLinear.Utility.Prelude

import qualified Data.Vector as V

import HLinear.Matrix.Definition ( Matrix(..) )
import qualified HLinear.Matrix.Basic as M


class IsFraction a n d | a -> n d where
  toFraction :: a -> (n, d)
  fromNumerator :: n -> a
  fromDenominator :: d -> a

instance IsFraction FMPQ FMPZ (NonZero FMPZ) where
  toFraction = toFMPZs
  fromNumerator n = fromFMPZs n one
  fromDenominator = fromFMPZs one . fromNonZero


instance
     ( IsFraction a n (NonZero d)
     , EuclideanDomain d, MultiplicativeSemigroupRightAction d n )
  => IsFraction (Vector a) (Vector n) (NonZero d)
  where
  toFraction v 
    | V.null v  = (mempty, NonZero one)
    | otherwise =
        let (ns,ds) = V.unzip $ fmap (second fromNonZero . toFraction) v
            den = foldr1 lcm ds
        in ( V.zipWith (\n d -> n .* (den `quot` d)) ns ds
           , NonZero den
           )
  fromNumerator = fmap fromNumerator
  fromDenominator = error "isFraction (Vector a): fromDenominator would require size"


instance
     ( IsFraction a n (NonZero d)
     , Ring a, EuclideanDomain d, MultiplicativeSemigroupRightAction d n )
  => IsFraction (Matrix a) (Matrix n) (Vector (NonZero d))
  where
  toFraction (Matrix nrs ncs rs) =
    first (Matrix nrs ncs) $ V.unzip $ fmap toFraction rs
  fromNumerator = fmap fromNumerator
  fromDenominator = M.diagonal . fmap fromDenominator

