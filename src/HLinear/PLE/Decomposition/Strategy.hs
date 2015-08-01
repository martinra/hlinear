module HLinear.PLE.Decomposition.Strategy
where

import HLinear.Matrix ( Matrix )
import HLinear.PLE.Decomposition.Definition
import HLinear.PLE.FoldUnfold.Echelonize
import HLinear.PLE.MultiMod.Echelonize.Definition
import HLinear.PLE.Sliced.Echelonize.Definition
import HLinear.PLE.Strategy


pleDecompositionWithStrategy ::
     PLEStrategy a
  -> Matrix a
  -> PLEDecomposition a
pleDecompositionWithStrategy FoldUnfold m =
  pleDecompositionFoldUnfold m
pleDecompositionWithStrategy (Sliced p) m =
  pleDecompositionSliced p m
pleDecompositionWithStrategy (MultiMod p) m =
  pleDecompositionMultiMod p m
