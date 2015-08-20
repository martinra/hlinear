{-# LANGUAGE
    TypeFamilies
  #-}

module HLinear.PLE.Decomposition.Definition
where


data family PLEDecomposition :: * -> *

class HasPLEDecomposition a where
  pleDecomposition :: a -> PLEDecomposition a
