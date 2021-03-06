module HLinear.Hook.PLEHook.Basic
where


import qualified Prelude as P
import HLinear.Utility.Prelude hiding ( one )

import qualified Math.Structure as MS

import HLinear.Hook.PLEHook.Definition ( PLEHook(..) )
import HLinear.Utility.RPermute ( RPermute, rpermute )
import qualified HLinear.Hook.EchelonForm.Basic as EF
import qualified HLinear.Hook.LeftTransformation.Basic as LT


--------------------------------------------------------------------------------
-- creation
--------------------------------------------------------------------------------

one :: Int -> Int -> PLEHook a b
one nrs ncs
  | nrs >= 0 && ncs >= 0 = PLEHook (rpermute nrs) (LT.one nrs) (EF.zero nrs ncs)
  | nrs < 0 = error "PLEHook.one: negative nrs"
  | ncs < 0 = error "PLEHook.one: negative ncs"
