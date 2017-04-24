{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module HLinear.PLE.FoldUnfold.FractionFree
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , lcm, gcd
                      , quotRem, quot, rem
                      )
import qualified Prelude as P

import Control.Arrow ( (&&&), first, second )
import Data.Permute ( Permute )
import Data.Vector ( Vector )
import Math.Structure
import Numeric.Natural ( Natural )
import System.IO.Unsafe ( unsafePerformIO )
import qualified Data.Vector as V

import HFlint.FMPQ ( FMPQ, toFMPZs, fromFMPZs )
import HFlint.FMPZ ( FMPZ )
import HFlint.FMPZ.FFI ( fmpz_mul, fmpz_submul, fmpz_divexact )
import HFlint.Internal ( withFlint, withNewFlint_ )


import HLinear.Hook.EchelonForm ( EchelonForm(..) )
import HLinear.Hook.LeftTransformation ( LeftTransformation(..) )
import HLinear.Hook.PLEHook ( PLEHook(..) )
import HLinear.Matrix ( Matrix(..), headRows, tailRows )
import HLinear.PLE.FoldUnfold.Matrix ( splitOffTopLeft )
import HLinear.Utility.RPermute ( RPermute(..) )
import qualified HLinear.Hook.EchelonForm as EF
import qualified HLinear.Hook.LeftTransformation as LT
import qualified HLinear.Hook.PLEHook.Basic as Hook
import qualified HLinear.Matrix as M
import qualified HLinear.PLE.FoldUnfold.DivisionRing as DR
import qualified HLinear.Utility.RPermute as RP


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


data MatrixFraction a n d where
  MatrixFraction
    :: IsFraction a n d 
    => Matrix n -> d -> MatrixFraction a n d


ple :: Matrix FMPQ -> PLEHook FMPQ
ple m@(Matrix nrs ncs rs) =
  let (mnum, ds) = toFraction m
  in  case splitOffHook (MatrixFraction mnum one) of
        Nothing -> Hook.one nrs ncs
        Just (h,m') ->
          PLEHook one
            (LT.diagonal $ V.map (Unit . fromDenominator) ds)
            (EF.zero nrs ncs)
          *
          V.foldl (*) h (V.unfoldr splitOffHook m')
  
splitOffHook
  :: MatrixFraction FMPQ FMPZ (NonZero FMPZ)
  -> Maybe (PLEHook FMPQ, MatrixFraction FMPQ FMPZ (NonZero FMPZ))
splitOffHook (MatrixFraction m@(Matrix nrs ncs rs) den)
  | nrs == 0 || ncs == 0 = Nothing
  | Just p <- DR.pivotPermutation m = Just $
      let Just ((pivot, pivotBottom), (pivotTail, bottomRight)) =
            splitOffTopLeft ( p *. m)

          denFMPQ = Unit $ fromDenominator den :: Unit FMPQ
          pivotRecip = recip $ Unit $ fromNumerator pivot :: Unit FMPQ

          lt = LT.singleton (denFMPQ * pivotRecip) $
                 V.map (\a -> fromUnit denFMPQ * fromNumerator a) $
                   V.map negate pivotBottom
          ef = EF.singletonLeadingOne nrs $
                 V.map (\a -> fromUnit pivotRecip * fromNumerator a)
                   pivotTail

          bottomRight' =
            (\f -> V.zipWith f pivotBottom bottomRight) $ \h t ->
              (\f' -> V.zipWith f' pivotTail t) $ \pv te ->
                mulSubMulDiv pivot te h pv den

          mulSubMulDiv :: FMPZ -> FMPZ -> FMPZ -> FMPZ -> NonZero FMPZ -> FMPZ
          mulSubMulDiv a a' b b' (NonZero c) = unsafePerformIO $
            withNewFlint_ $ \dptr  ->
            withFlint a   $ \aptr  ->
            withFlint a'  $ \a'ptr ->
            withFlint b   $ \bptr  ->
            withFlint b'  $ \b'ptr ->
            withFlint c   $ \cptr  -> do
              fmpz_mul dptr aptr a'ptr
              fmpz_submul dptr bptr b'ptr
              fmpz_divexact dptr dptr cptr
      in  ( PLEHook p lt ef
          , MatrixFraction (Matrix (nrs P.- 1) (ncs P.- 1) bottomRight') den
          )
  | otherwise            = Just $
      ( Hook.one nrs ncs
      , MatrixFraction (Matrix nrs (ncs P.- 1) $ V.map V.tail rs) den
      )
