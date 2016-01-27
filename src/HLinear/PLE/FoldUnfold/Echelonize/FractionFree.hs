{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module HLinear.PLE.FoldUnfold.Echelonize.FractionFree
  (
  )
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , lcm, gcd
                      , quotRem, quot, rem
                      )

import Control.Arrow ( (&&&) )
import Data.Permute ( Permute )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural
import System.IO.Unsafe ( unsafePerformIO )

import HFlint.FMPQ
import HFlint.FMPZ
import HFlint.FMPZ.FFI ( fmpz_mul, fmpz_submul, fmpz_divexact )
import HFlint.Internal ( withFlint, withNewFlint_ )


import HLinear.PLE.Decomposition.Definition
import HLinear.PLE.Decomposition.Matrix
import HLinear.PLE.FoldUnfold.Echelonize.Definition
import HLinear.PLE.Hook
import HLinear.PLE.Hook.PLMatrix
import qualified HLinear.PLE.Hook.RPermute as RP
import HLinear.PLE.Hook.RPermute ( RPermute(..) )
import qualified HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.EchelonForm ( EchelonForm(..) )
import qualified HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.PLE.Hook.LeftTransformation ( LeftTransformation(..) )
import HLinear.Matrix ( Matrix(..), headRows, tailRows )
import qualified HLinear.Matrix as M


class HasFractionFree a n d | a -> n d where
  toFractionFree :: a -> (n,d)

  fromNumerator :: n -> a
  fromDenominator :: d -> a

instance HasFractionFree FMPQ FMPZ (NonZero FMPZ) where
  toFractionFree = toFMPZs

  fromNumerator n = fromFMPZs n one
  fromDenominator = fromFMPZs one . fromNonZero

toFractionFreeVector
  :: ( EuclideanDomain d, HasFractionFree a n (NonZero d), MultiplicativeSemigroupRightAction d n )
  => Vector a -> (Vector n, NonZero d)
toFractionFreeVector as =
  let (ns,ds) = V.unzip $ V.map toFractionFree as
      den = V.foldr lcm one $ V.map fromNonZero ds
  in (V.zipWith (\n d -> n .* (den `quot` fromNonZero d)) ns ds, NonZero den)

toFractionFreeMatrixRowwise
  :: ( EuclideanDomain d, HasFractionFree a n (NonZero d), MultiplicativeSemigroupRightAction d n )
  => Matrix a -> (Matrix n, Vector (NonZero d))
toFractionFreeMatrixRowwise (Matrix nrs ncs rs) = 
  let (rsn,ds) = V.unzip $ V.map toFractionFreeVector rs
  in (Matrix nrs ncs rsn,ds)


data MatrixFractionFree a n d where
  MatrixFractionFree
    :: HasFractionFree a n d 
    => Matrix n -> d -> MatrixFractionFree a n d


instance HasPLEDecompositionFoldUnfoldFractionFree (Matrix FMPQ) where
  pleDecompositionFoldUnfoldFractionFree m@(Matrix nrs ncs rs) =
    PLEDecomposition $
      V.foldl (*)
      ( PLEHook one
                ( LT.fromDiagonal $ V.map (NonZero . fromDenominator) ds )
                ( EF.zeroEF nrs ncs) )
      ( V.unfoldr splitOffHook (MatrixFractionFree mnum one) )
    where
      (mnum :: Matrix FMPZ, ds :: Vector (NonZero FMPZ)) = toFractionFreeMatrixRowwise m

splitOffHook
  :: MatrixFractionFree FMPQ FMPZ (NonZero FMPZ)
  -> Maybe (PLEHook FMPQ, MatrixFractionFree FMPQ FMPZ (NonZero FMPZ))
splitOffHook (MatrixFractionFree m@(Matrix nrs ncs rs) den)
  | nrs == 0 || ncs == 0 = Nothing
  | otherwise            = Just $
      case V.findIndex ((not . isZero) . V.head) rs of
        Nothing  -> ( PLEHook one (LT.identityLT nrs) $ EF.zeroEF nrs ncs
                    , MatrixFractionFree (Matrix nrs (pred ncs) $ V.map V.tail rs) den
                    )
        Just pIx -> ( PLEHook p lt ef
                    , MatrixFractionFree (Matrix (pred nrs) (pred ncs) matRows) (NonZero pivot)
                    )
          where
          denFMPQ = NonZero $ fromDenominator den :: NonZero FMPQ
          pivotRow = rs V.! pIx
          pivot = V.head pivotRow
          pivotRecip = recip $ NonZero $ fromNumerator pivot :: NonZero FMPQ
          pivotTail = V.tail pivotRow

          bottomRows =
            if pIx == 0
            then V.tail rs
            else V.update (V.tail rs) $ V.singleton (pred pIx,V.head rs)
          (bottomHeads,bottomTails) =
            V.unzip $ V.map (V.head &&& V.tail) bottomRows

          p = RP.fromTransposition (fromIntegral nrs) (0,pIx)
          lt = LeftTransformation nrs $ V.singleton $
              LT.LeftTransformationColumn 0
                ( denFMPQ * pivotRecip )
                ( V.map (\a -> fromNonZero denFMPQ * fromNumerator a )
                        ( V.map negate bottomHeads ) )
          ef = EF.singletonLeadingOne nrs 0 $
                 V.map ( \a -> fromNonZero pivotRecip * fromNumerator a )
                       ( V.tail pivotRow )

          matRows = V.zipWith
                      ( \h t -> V.zipWith (\pv te -> mulSubMulDiv pivot te h pv (fromNonZero den)) pivotTail t )
                      bottomHeads bottomTails

          mulSubMulDiv :: FMPZ -> FMPZ -> FMPZ -> FMPZ -> FMPZ -> FMPZ
          mulSubMulDiv a a' b b' c = unsafePerformIO $
            withNewFlint_ $ \dptr  ->
            withFlint a   $ \aptr  ->
            withFlint a'  $ \a'ptr ->
            withFlint b   $ \bptr  ->
            withFlint b'  $ \b'ptr ->
            withFlint c   $ \cptr  -> do
              fmpz_mul dptr aptr a'ptr
              fmpz_submul dptr bptr b'ptr
              fmpz_divexact dptr dptr cptr
