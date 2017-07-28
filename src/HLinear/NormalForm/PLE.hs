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
-- import qualified HLinear.NormalForm.FoldUnfold.PLE.FractionFree as FUFF

-- todo: instead of FF as a standard choice, there should be a
--       heuristic ple that chooses between the various
--       implementations and is inserted by the rule system
-- {-# RULES
--   "ple/FMPQ"  ple = FUFF.ple :: Matrix FMPQ -> PLEHook FMPQ
--   #-}
