{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module HLinear.PLE.FoldUnfold.Echelonize.Definition
where

import HLinear.PLE.Decomposition.Definition


class HasPLEDecompositionFoldUnfold a where
  pleDecompositionFoldUnfold
    :: a -> PLEDecomposition a

