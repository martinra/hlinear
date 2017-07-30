module HLinear.NormalForm.FoldUnfold.ReduceEchelonForm.FractionFree
where

import HLinear.Utility.Prelude

import qualified Data.Vector as V
import HFlint.FMPZ.FFI ( fmpz_mul, fmpz_submul, fmpz_divexact )

import HLinear.Hook.EchelonForm ( EchelonForm(..) )
import HLinear.Hook.EchelonForm.Row ( EchelonFormRow(..) )
import HLinear.Utility.Fraction ( Fraction(..) )
import qualified HLinear.Hook.EchelonForm as EF
import qualified HLinear.Hook.EchelonForm.Row as EFR


-- todo: We use in the implementation that the echelon form has pivots at
-- offsets, and no empty rows. This is enforced by the fraction free ple, but
-- should be ensured in general.
{-# INLINABLE reduceEchelonForm #-}
reduceEchelonForm
  :: EchelonForm FMPZ -> Fraction (EchelonForm FMPZ) (NonZero FMPZ)
reduceEchelonForm ef@(EchelonForm nrs ncs rs)
  | V.null rs = Fraction ef one
  | otherwise  = Fraction (EchelonForm nrs ncs rs') (NonZero den)
      where
        rs' = foldr snocNormalized mempty rs
        den = pivot $ V.last rs

        pivot r = let EchelonFormRow _ efr = r in V.head efr

        snocNormalized r rsnorm
          | V.null rsnorm = V.singleton r
          | otherwise     = (`V.cons` rsnorm) $ 
              let rden = pivot r
                  (EchelonFormRow o' r', s') = foldr normalize (r,ncs) rsnorm
                  (r'1,r'2) = V.splitAt (s'-o'-1) (V.tail r')
              in  EchelonFormRow o' $ den `V.cons`
                    fmap (`divexactFMPZ` rden) (fmap (*den) r'1 <> r'2)

        normalize (EchelonFormRow on rn) (EchelonFormRow o r, s) =
          (EchelonFormRow o $ r1 <> zero `V.cons` r3' <> r4', on)
          where
            (r1,r234) = V.splitAt (on-o) r
            r34 = V.tail r234
            (r23,r4) = V.splitAt (s-on) r234
            r2 = V.head r23
            r3 = V.tail r23
            (rn3,rn4) = V.splitAt (s-on-1) (V.tail rn)
            r3' = (\f -> V.zipWith f r3 rn3) $ \a an -> a*den - an*r2
            r4' = (\f -> V.zipWith f r4 rn4) $ \a an -> a - an*r2
