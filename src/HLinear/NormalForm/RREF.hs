{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeFamilies
  , UndecidableInstances
  #-}

module HLinear.NormalForm.RREF
where

import Prelude ()
import HLinear.Utility.Prelude

import HLinear.Hook.PLEHook ( PLREHook(..) )
import HLinear.Matrix.Definition ( Matrix )


class HasRREF a where
  rref :: Matrix a -> PLREHook a
