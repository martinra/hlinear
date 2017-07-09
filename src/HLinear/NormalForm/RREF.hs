module HLinear.NormalForm.RREF
  ( rref, HasRREF
  , RREF(..)
  )
where

import HLinear.Utility.Prelude

import HLinear.Hook.PLEHook ( PLUEHook(..) )
import HLinear.NormalForm.FoldUnfold.RREF.DivisionRing ( rref, HasRREF )


type RREF a = PLUEHook a
