module HLinear.Hook.ERHook.Basic
where

import HLinear.Utility.Prelude


import HLinear.Hook.ERHook.Definition ( ERHook(..) )
import HLinear.Matrix.Definition ( Matrix(..) )
import qualified HLinear.Hook.EchelonForm.Basic as EF
import qualified HLinear.Hook.EchelonTransformation.Basic as ET


--------------------------------------------------------------------------------
-- creation
--------------------------------------------------------------------------------

one :: Int -> Int -> ERHook a
one nrs ncs
  | nrs >= 0 && ncs >= 0 = ERHook (ET.one nrs) (Matrix 0 ncs mempty) (EF.zero nrs ncs)
  | nrs < 0 = error "ERHook.zero: negative nrs"
  | ncs < 0 = error "ERHook.zero: negative ncs"
