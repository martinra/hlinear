module HLinear.Hook.ERHook.Basic
where

import Numeric.Natural ( Natural )
import qualified Data.Vector as V
import qualified Math.Structure as MS

import HLinear.Hook.ERHook.Definition ( ERHook(..) )
import HLinear.Matrix ( Matrix(..) )
import qualified HLinear.Hook.EchelonForm.Basic as EF
import qualified HLinear.Hook.EchelonTransformation.Basic as ET


one :: Natural -> Natural -> ERHook a
one nrs ncs = ERHook (ET.one nrs) (Matrix 0 ncs V.empty) (EF.zero nrs ncs)
