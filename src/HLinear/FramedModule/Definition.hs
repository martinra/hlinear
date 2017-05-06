module HLinear.FramedModule.Definition
where

import Prelude ()
import HLinear.Utility.Prelude

import HLinear.Matrix.Definition ( Matrix(..) )
import HLinear.Hook.EchelonForm ( EchelonForm(..) )


-- we work with echelon forms of row matrices
data FramedModule a =
    FramedModuleBasis (EchelonForm a) (Vector Int)
  | FramedModuleDual  (EchelonForm a) (Vector Int)
  deriving ( Show )
