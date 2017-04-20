{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , TypeSynonymInstances
  #-}

module HLinear.Matrix.Invertible
where

import Math.Structure ( Unit, fromUnit )

import HLinear.Matrix.Definition


type MatrixInvertible a = Unit (Matrix a)

instance IsMatrix (MatrixInvertible a) a where
  toMatrix = fromUnit
