module HLinear.NormalForm.FoldUnfold.RREF.DivisionRing
where

import qualified Prelude as P
import HLinear.Utility.Prelude

import HLinear.Matrix.Definition ( Matrix )
import HLinear.Hook.PLEHook.Definition ( PLEHook(..), PLUEHook(..), UEHook(..) )
import HLinear.NormalForm.FoldUnfold.PLE.DivisionRing ( HasPLE )
import HLinear.NormalForm.FoldUnfold.ReduceEchelonForm.DivisionRing ( reduceEchelonForm )
import qualified HLinear.NormalForm.FoldUnfold.PLE.DivisionRing as PLEDR


type HasRREF a = ( DivisionRing a, DecidableZero a, HasPLE a )


{-# INLINABLE rref #-}
rref :: HasRREF a => Matrix a -> PLUEHook a
rref = rrefWithPLE PLEDR.ple

{-# INLINABLE rrefWithPLE #-}
rrefWithPLE
  :: ( DivisionRing a, DecidableZero a )
  => ( Matrix a -> PLEHook a )
  -> Matrix a -> PLUEHook a
rrefWithPLE ple m =
  let h@(PLEHook p l e) = ple m
      UEHook u e' = reduceEchelonForm e
  in  PLUEHook p l u e'
