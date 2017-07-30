module HLinear.Utility.FMPZ
where

import HLinear.Utility.Prelude

import HFlint.FMPZ.FFI ( fmpz_set, fmpz_mul, fmpz_submul, fmpz_divexact )
import HFlint.Internal ( withFlint_, withNewFlint_ )


{-# INLINE subMul #-}
subMul
  :: FMPZ -> FMPZ -> FMPZ -> FMPZ
subMul a b c = unsafePerformIO $
  withNewFlint_ $ \rptr ->
  withFlint_ a $ \aptr ->
  withFlint_ b $ \bptr ->
  withFlint_ c $ \cptr -> do
    fmpz_set rptr aptr
    fmpz_submul rptr bptr cptr

{-# INLINE mulSubMul #-}
mulSubMul
  :: FMPZ -> FMPZ -> FMPZ -> FMPZ -> FMPZ
mulSubMul a b c d = unsafePerformIO $
  withNewFlint_ $ \rptr ->
  withFlint_ a $ \aptr ->
  withFlint_ b $ \bptr ->
  withFlint_ c $ \cptr ->
  withFlint_ d $ \dptr -> do
    fmpz_mul rptr aptr bptr
    fmpz_submul rptr cptr dptr

{-# INLINE mulSubMulDivexact #-}
mulSubMulDivexact
  :: FMPZ -> FMPZ -> FMPZ -> FMPZ -> FMPZ -> FMPZ
mulSubMulDivexact a b c d e = unsafePerformIO $
  withNewFlint_ $ \rptr ->
  withFlint_ a $ \aptr ->
  withFlint_ b $ \bptr ->
  withFlint_ c $ \cptr ->
  withFlint_ d $ \dptr ->
  withFlint_ e $ \eptr -> do
    fmpz_mul rptr aptr bptr
    fmpz_submul rptr cptr dptr
    fmpz_divexact rptr rptr eptr
