{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module HLinear.NormalForm.FoldUnfold.PLE.FractionFree
where

import HLinear.Utility.Prelude

import Data.Permute ( Permute )
import qualified Data.Vector as V

import HFlint.FMPZ.FFI ( fmpz_mul, fmpz_submul, fmpz_divexact )
import HFlint.Internal ( withFlint, withNewFlint_ )


import HLinear.Hook.EchelonForm ( EchelonForm(..) )
import HLinear.Hook.LeftTransformation ( LeftTransformation(..) )
import HLinear.Hook.PLEHook ( PLEHook(..) )
import HLinear.Matrix.Definition ( Matrix(..) )
import HLinear.Matrix.Block ( headRows, tailRows )
import HLinear.NormalForm.FoldUnfold.Fraction ( IsFraction(..) )
import HLinear.NormalForm.FoldUnfold.Pivot ( splitOffPivotNonZero )
import HLinear.Utility.RPermute ( RPermute(..) )
import qualified HLinear.Hook.EchelonForm as EF
import qualified HLinear.Hook.LeftTransformation as LT
import qualified HLinear.Hook.PLEHook.Basic as Hook
import qualified HLinear.Utility.RPermute as RP


data MatrixFraction a n d where
  MatrixFraction
    :: IsFraction a n d 
    => Matrix n -> d -> MatrixFraction a n d


ple :: Matrix FMPQ -> PLEHook FMPQ
ple m@(Matrix nrs ncs rs) =
  -- todo: toFraction takes too much time: look into how flint does that
  let (mnum, ds) = toFraction m
  in  case splitOffHook (MatrixFraction mnum one) of
        Nothing -> Hook.one nrs ncs
        Just (h,m') ->
          PLEHook one
            (LT.diagonal $ fmap (Unit . fromDenominator) ds)
            (EF.zero nrs ncs)
          *
          V.foldl (*) h (V.unfoldr splitOffHook m')
  
splitOffHook
  :: MatrixFraction FMPQ FMPZ (NonZero FMPZ)
  -> Maybe (PLEHook FMPQ, MatrixFraction FMPQ FMPZ (NonZero FMPZ))
splitOffHook (MatrixFraction m@(Matrix nrs ncs rs) den)
  | nrs == 0 || ncs == 0 = Nothing
  | otherwise = Just $ case splitOffPivotNonZero m of
      Nothing ->
        ( Hook.one nrs ncs
        , MatrixFraction (Matrix nrs (ncs-1) $ fmap V.tail rs) den
        )
      Just (p, ((pivot, pivotBottom), (pivotTail, bottomRight))) ->
        ( PLEHook p lt ef
        , MatrixFraction (Matrix (nrs-1) (ncs-1) bottomRight') den
        )
        where
          lt = LT.singleton (Unit $ fromDenominator $ den * pivot) $
                 fmap (\a -> fromFraction a den) $
                   fmap negate pivotBottom
          ef = EF.singletonLeadingOne nrs $
                 fromFraction pivotTail pivot

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
