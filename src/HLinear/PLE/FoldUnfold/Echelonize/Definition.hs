{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module HLinear.PLE.FoldUnfold.Echelonize.Definition
where

import HLinear.PLE.Decomposition.Definition


class HasPLEDecompositionFoldUnfold f a where
  pleDecompositionFoldUnfold
    :: f a -> PLEDecomposition a

