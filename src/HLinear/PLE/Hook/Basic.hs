module HLinear.PLE.Hook.Basic
where


import Numeric.Natural ( Natural )
import qualified Math.Structure as MS

import HLinear.PLE.Hook.Definition ( PLEHook(..) )
import HLinear.Utility.RPermute ( RPermute )
import qualified HLinear.PLE.Hook.EchelonForm.Basic as EF
import qualified HLinear.PLE.Hook.LeftTransformation.Basic as LT


one :: Natural -> Natural -> PLEHook a
one nrs ncs = PLEHook MS.one (LT.one nrs) (EF.zero nrs ncs)
