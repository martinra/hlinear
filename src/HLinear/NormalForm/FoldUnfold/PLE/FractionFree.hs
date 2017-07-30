{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module HLinear.NormalForm.FoldUnfold.PLE.FractionFree
where

import HLinear.Utility.Prelude

import qualified Data.Vector as V

import HLinear.Hook.EchelonForm ( EchelonForm(..) )
import HLinear.Matrix.Definition ( Matrix(..) )
import HLinear.Matrix.Fraction ()
import HLinear.NormalForm.FoldUnfold.Pivot ( splitOffPivotNonZero )
import HLinear.Utility.FMPZ ( mulSubMulDivexact )
import HLinear.Utility.Fraction ( IsFraction(..), Fraction(..) )
import qualified HLinear.Hook.EchelonForm as EF


ple :: Matrix FMPQ -> EchelonForm FMPZ
ple m@(Matrix nrs ncs _) =
  let Fraction mnum _ = toFraction m
  in case splitOffHook (mnum, one) of
       Nothing -> EF.zero nrs ncs
       Just (h,m') ->
         EF.zero nrs ncs
         +
         V.foldl (+) h (V.unfoldr splitOffHook m')
  
splitOffHook
  :: (Matrix FMPZ, FMPZ)
  -> Maybe (EchelonForm FMPZ, (Matrix FMPZ, FMPZ))
splitOffHook (m@(Matrix nrs ncs rs), prevPivot)
  | nrs == 0 || ncs == 0 = Nothing
  | otherwise = Just $ case splitOffPivotNonZero m of
      Nothing ->
        ( EF.zero nrs ncs
        , (Matrix nrs (ncs-1) (fmap V.tail rs), prevPivot)
        )
      Just (_, ((NonZero pivot, pivotBottom), (pivotTail, bottomRight))) ->
        ( ef
        , (Matrix (nrs-1) (ncs-1) bottomRight', pivot)
        )
        where
          ef = EF.singleton nrs $ pivot `V.cons` pivotTail

          bottomRight' =
            (\f -> V.zipWith f pivotBottom bottomRight) $ \h t ->
              (\f' -> V.zipWith f' pivotTail t) $ \pv te ->
                mulSubMulDivexact pivot te h pv prevPivot
