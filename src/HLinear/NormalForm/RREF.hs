module HLinear.NormalForm.RREF
  ( rref, HasRREF
  , PLREHook(..)
  )
where

import Prelude ()
import HLinear.Utility.Prelude

import HLinear.Hook.PLEHook ( PLREHook(..) )
import HLinear.NormalForm.FoldUnfold.ReduceEchelonForm.DivisionRing ( rref, HasRREF )
