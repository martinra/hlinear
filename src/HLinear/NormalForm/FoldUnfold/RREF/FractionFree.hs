module HLinear.NormalForm.FoldUnfold.RREF.FractionFree
where

import qualified Prelude as P
import HLinear.Utility.Prelude

import HLinear.Matrix.Definition ( Matrix )
import HLinear.Hook.EchelonForm.Definition ( EchelonForm )
import HLinear.NormalForm.FoldUnfold.ReduceEchelonForm.FractionFree ( reduceEchelonForm )
import HLinear.Utility.Fraction ( Fraction )
import qualified HLinear.NormalForm.FoldUnfold.PLE.FractionFree as PLEFF


{-# INLINABLE rref #-}
rref :: Matrix FMPQ -> Fraction (EchelonForm FMPZ) (NonZero FMPZ)
rref = rrefWithPLE PLEFF.ple

{-# INLINABLE rrefWithPLE #-}
rrefWithPLE
  :: ( Matrix FMPQ -> EchelonForm FMPZ )
  -> Matrix FMPQ -> Fraction (EchelonForm FMPZ) (NonZero FMPZ)
rrefWithPLE ple = reduceEchelonForm . ple
