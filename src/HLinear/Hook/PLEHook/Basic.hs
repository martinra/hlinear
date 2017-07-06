module HLinear.Hook.PLEHook.Basic
where


import qualified Prelude as P
import HLinear.Utility.Prelude hiding ( one )

import qualified Math.Structure as MS

import HLinear.Hook.PLEHook.Definition ( PLEHook(..) )
import HLinear.Utility.RPermute ( RPermute, rpermute )
import qualified HLinear.Hook.EchelonForm.Basic as EF
import qualified HLinear.Hook.LeftTransformation.Basic as LT


one :: Natural -> Natural -> PLEHook a
one nrs ncs = PLEHook (rpermute $ fromIntegral nrs) (LT.one nrs) (EF.zero nrs ncs)

--------------------------------------------------------------------------------
-- container
--------------------------------------------------------------------------------

instance Functor PLEHook where
  fmap = fmapDefault

instance Foldable PLEHook where
  foldMap = foldMapDefault

instance Traversable PLEHook where
  traverse f (PLEHook p l e) = PLEHook p <$> traverse f l <*> traverse f e
