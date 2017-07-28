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
          | otherwise = (`V.cons` rsnorm) $ foldr (normalize $ pivot r) (fmap (*den) r) rsnorm

        normalize rden (EchelonFormRow on rn) (EchelonFormRow o r) =
          EchelonFormRow o $ r1 <> zero `V.cons` r3'
          where
            (r1,r23) = V.splitAt (o-on) r
            r2 = V.head r23
            r3 = V.tail r23
            r3' = (\f -> V.zipWith f r3 (V.tail rn)) $ \a an -> unsafePerformIO $
              withNewFMPZ_ $ \bptr ->
              withFMPZ_ a  $ \aptr ->
              withFMPZ_ an $ \anptr ->
              withFMPZ_ r2 $ \r2ptr ->
              withFMPZ_ rden $ \rdenptr ->
              withFMPZ_ den  $ \denptr -> do
                fmpz_mul bptr aptr denptr
                fmpz_submul bptr anptr r2ptr
                fmpz_divexact bptr anptr rdenptr
