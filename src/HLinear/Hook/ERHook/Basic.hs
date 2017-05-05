module HLinear.Hook.ERHook.Basic
where

import Prelude ()
import HLinear.Utility.Prelude


import HLinear.Hook.ERHook.Definition ( ERHook(..) )
import HLinear.Matrix.Definition ( Matrix(..) )
import qualified HLinear.Hook.EchelonForm.Basic as EF
import qualified HLinear.Hook.EchelonTransformation.Basic as ET


one :: Natural -> Natural -> ERHook a
one nrs ncs = ERHook (ET.one nrs) (Matrix 0 ncs mempty) (EF.zero nrs ncs)
