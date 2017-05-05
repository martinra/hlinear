module HLinear.NormalForm.PLE
where

import Prelude ()
import HLinear.Utility.Prelude

import HLinear.Hook.PLEHook ( PLEHook(..) )
import HLinear.Matrix.Definition ( Matrix )


class HasPLE a where
  ple :: Matrix a -> PLEHook a
