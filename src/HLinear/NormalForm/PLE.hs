module HLinear.NormalForm.PLE
  ( ple, HasPLE
  , PLEHook(..)
  )
where

import HLinear.Utility.Prelude

import HFlint.FMPQ

import HLinear.Hook.PLEHook ( PLEHook(..) )
import HLinear.Matrix.Definition ( Matrix )
import HLinear.NormalForm.FoldUnfold.PLE.DivisionRing ( ple, HasPLE )
import qualified HLinear.NormalForm.FoldUnfold.PLE.FractionFree as FUFF

{-# RULES
  "ple/FMPQ"  ple = FUFF.ple :: Matrix FMPQ -> PLEHook FMPQ
  #-}
