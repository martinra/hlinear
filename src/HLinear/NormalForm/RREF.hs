module HLinear.NormalForm.RREF
  ( rref, HasRREF
  , PLUEHook(..)
  )
where

import HLinear.Utility.Prelude

import HLinear.Hook.PLEHook ( PLUEHook(..) )
import HLinear.NormalForm.FoldUnfold.ReduceEchelonForm.DivisionRing ( rref, HasRREF )
