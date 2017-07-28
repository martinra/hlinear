{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module HLinear.NormalForm.FoldUnfold.PLE.FractionFree
where

import HLinear.Utility.Prelude

import qualified Data.Vector as V

import HFlint.FMPZ.FFI ( fmpz_mul, fmpz_submul, fmpz_divexact )
import HFlint.Internal ( withFlint, withNewFlint_ )

import HLinear.Hook.EchelonForm ( EchelonForm(..) )
import HLinear.Matrix.Definition ( Matrix(..) )
import HLinear.Matrix.Fraction ()
import HLinear.NormalForm.FoldUnfold.Pivot ( splitOffPivotNonZero )
import HLinear.Utility.Fraction ( IsFraction(..), Fraction(..) )
import qualified HLinear.Hook.EchelonForm as EF


data MatrixFraction a n d where
  MatrixFraction
    :: IsFraction a n d 
    => Matrix n -> d -> MatrixFraction a n d


ple :: Matrix FMPQ -> EchelonForm FMPZ
ple m@(Matrix nrs ncs _) =
  let Fraction mnum _ = toFraction m
  in case splitOffHook (Fraction mnum one) of
       Nothing -> EF.zero nrs ncs
       Just (h,m') ->
         EF.zero nrs ncs
         +
         V.foldl (+) h (V.unfoldr splitOffHook m')
  
splitOffHook
  :: Fraction (Matrix FMPZ) (NonZero FMPZ)
  -> Maybe (EchelonForm FMPZ, Fraction (Matrix FMPZ) (NonZero FMPZ))
splitOffHook (Fraction m@(Matrix nrs ncs rs) den)
  | nrs == 0 || ncs == 0 = Nothing
  | otherwise = Just $ case splitOffPivotNonZero m of
      Nothing ->
        ( EF.zero nrs ncs
        , Fraction (Matrix nrs (ncs-1) $ fmap V.tail rs) den
        )
      Just (_, ((pivot, pivotBottom), (pivotTail, bottomRight))) ->
        ( ef
        , Fraction (Matrix (nrs-1) (ncs-1) bottomRight') pivot
        )
        where
          ef = EF.singleton nrs $ fromNonZero pivot `V.cons` pivotTail

          bottomRight' =
            (\f -> V.zipWith f pivotBottom bottomRight) $ \h t ->
              (\f' -> V.zipWith f' pivotTail t) $ \pv te ->
                mulSubMulDiv (fromNonZero pivot) te h pv den

          -- todo: to implement this to arbitarary IsFraction a n (NonZero d),
          -- we have to find a quotient n `quot` d; Possibly this should be part of the
          -- definition of IsFraction
          -- ((a*a') - (b*b')) `quot` c
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
