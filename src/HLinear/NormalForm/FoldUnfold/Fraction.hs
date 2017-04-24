{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}


module HLinear.NormalForm.FoldUnfold.Fraction
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , lcm, gcd
                      , quotRem, quot, rem
                      )

import Control.Arrow ( first, second )
import Data.Vector ( Vector )
import Math.Structure
import Numeric.Natural ( Natural )
import qualified Data.Vector as V

import HFlint.FMPQ ( FMPQ, toFMPZs, fromFMPZs )
import HFlint.FMPZ ( FMPZ )

import HLinear.Matrix ( Matrix(..) )
import qualified HLinear.Matrix as M


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
    | V.null v  = (V.empty, NonZero one)
    | otherwise =
        let (ns,ds) = V.unzip $ V.map (second fromNonZero . toFraction) v
            den = V.foldr1 lcm ds
        in ( V.zipWith (\n d -> n .* (den `quot` d)) ns ds
           , NonZero den
           )
  fromNumerator = V.map fromNumerator
  fromDenominator = undefined


instance
     ( IsFraction a n (NonZero d)
     , Ring a, EuclideanDomain d, MultiplicativeSemigroupRightAction d n )
  => IsFraction (Matrix a) (Matrix n) (Vector (NonZero d))
  where
  toFraction (Matrix nrs ncs rs) =
    first (Matrix nrs ncs) $ V.unzip $ V.map toFraction rs
  fromNumerator = fmap fromNumerator
  fromDenominator = M.diagonal . fmap fromDenominator

