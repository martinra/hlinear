{-# LANGUAGE
    FlexibleContexts
  , TypeFamilies
  #-}

module HLinear.NormalForm.PLH
where

import Prelude ()
import HLinear.Utility.Prelude

import HLinear.Matrix.Definition ( Matrix )
import HLinear.Hook.PLEHook ( PLREHook(..) )


class HasPLH a where
  plh :: Matrix a -> PLREHook a
