{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module HLinear.NormalForm.FoldUnfold.Fraction
where

import HLinear.Utility.Prelude

import HFlint.FMPZ.FFI ( fmpz_divexact, fmpz_mul )
import qualified Data.Vector as V

import HLinear.Matrix.Definition ( Matrix(..) )
import qualified HLinear.Matrix.Basic as M


class IsFraction a n d | a -> n d where
  toFraction :: a -> (n, d)
  fromFraction :: n -> d -> a
  fromNumerator :: n -> a
  fromDenominator :: d -> a

instance IsFraction FMPQ FMPZ (NonZero FMPZ) where
  {-# INLINABLE toFraction #-}
  toFraction = toFMPZs
  {-# INLINABLE fromFraction #-}
  fromFraction n (NonZero d) = fromFMPZs n d
  {-# INLINABLE fromNumerator #-}
  fromNumerator n = fromFMPZs n one
  {-# INLINABLE fromDenominator #-}
  fromDenominator = fromFMPZs one . fromNonZero


instance
     ( IsFraction a n (NonZero d)
     , EuclideanDomain d, MultiplicativeSemigroupRightAction d n )
  => IsFraction (Vector a) (Vector n) (NonZero d)
  where
  {-# NOINLINE[1] toFraction #-}
  toFraction v 
    | V.null v  = (mempty, NonZero one)
    | otherwise =
        let den = foldr1 lcm ds
            (ns,ds) = V.unzip $ (`fmap` v) $ \a ->
                        let (n, NonZero d) = toFraction a
                        in  (,d) $ n .* (den `quot` d)
        in  (ns, NonZero den)
  {-# INLINABLE fromFraction #-}
  fromFraction v d = fmap (`fromFraction` d) v
  {-# INLINABLE fromNumerator #-}
  fromNumerator = fmap fromNumerator
  {-# INLINABLE fromDenominator #-}
  fromDenominator = error "isFraction (Vector a): fromDenominator would require size"

instance
     ( IsFraction a n (NonZero d)
     , Ring a, EuclideanDomain d, MultiplicativeSemigroupRightAction d n )
  => IsFraction (Matrix a) (Matrix n) (Vector (NonZero d))
  where
  {-# INLINABLE toFraction #-}
  toFraction (Matrix nrs ncs rs) =
    first (Matrix nrs ncs) $ V.unzip $ fmap toFraction rs
  {-# INLINABLE fromFraction #-}
  fromFraction (Matrix nrs ncs rs) ds =
    Matrix nrs ncs $ V.zipWith fromFraction rs ds
  {-# INLINABLE fromNumerator #-}
  fromNumerator = fmap fromNumerator
  {-# INLINABLE fromDenominator #-}
  fromDenominator = M.diagonal . fmap fromDenominator


{-# RULES
  "toFraction/Vector FMPQ"  toFraction = toFractionVectorFMPQ
  #-}
{-# INLINABLE toFractionVectorFMPQ #-}
toFractionVectorFMPQ :: Vector FMPQ -> (Vector FMPZ, NonZero FMPZ)
toFractionVectorFMPQ v
  | V.null v  = (mempty, NonZero one)
  | otherwise =
      let den = foldr1 lcm ds
          (ns,ds) = V.unzip $ fmap (normalize . toFMPZs) v
          normalize (n, NonZero d) = (,d) $ unsafePerformIO $
            withNewFMPZ_ $ \bptr ->
            withFMPZ_ den $ \denptr ->
            withFMPZ_ d   $ \dptr ->
            withFMPZ_ n   $ \nptr -> do
              fmpz_divexact bptr denptr dptr
              fmpz_mul bptr bptr nptr
      in  (ns, NonZero den)
