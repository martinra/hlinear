module HLinear.Matrix.Invertible
where

import HLinear.Utility.Prelude

import qualified Data.Vector as V

import HLinear.Matrix.Algebra ()
import HLinear.Matrix.Definition
import HLinear.Hook.PLEHook.Definition ( PLEHook(..), PLREHook(..) )
import HLinear.Hook.EchelonForm.PivotStructure ( hasUnitDiagonal, pivotEntryVector )
import HLinear.NormalForm.PLE
import HLinear.NormalForm.RREF


type MatrixInvertible a = Unit (Matrix a)

instance IsMatrix (MatrixInvertible a) a where
  toMatrix = fromUnit

deriving instance HasNmbRows (Unit (Matrix a))
deriving instance HasNmbCols (Unit (Matrix a))

instance
     ( Ring a, DecidableZero a, DecidableUnit a, HasPLE a )
  => DecidableUnit (Matrix a)
  where
  toUnit = Unit
  isUnit m@(Matrix nrs ncs _)
    | nrs == ncs = let PLEHook _ _ e  = ple m in hasUnitDiagonal e
    | otherwise  = False

instance
     ( Ring a, DecidableZero a, DecidableUnit a, HasRREF a )
  => MultiplicativeGroup (Unit (Matrix a))
  where
  recip (Unit m) =
    let PLREHook p l r e = rref m
        (Matrix nrs ncs rs') = ((recip r) *. (toMatrix $ recip l)) .* (recip p)
    in  Unit $ Matrix nrs ncs $
          V.zipWith (\a -> fmap ((fromUnit $ recip $ toUnit a) *)) (pivotEntryVector e) rs'
