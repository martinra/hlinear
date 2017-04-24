{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HLinear.Hook.PLEHook.Definition
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.DeepSeq ( NFData(..) )
import Math.Structure

import HLinear.Matrix ( Matrix, IsMatrix(..) )
import HLinear.Hook.EchelonForm as EF
import HLinear.Hook.EchelonTransformation as ET
import HLinear.Hook.LeftTransformation as LT
import HLinear.Utility.RPermute as RP


data PLEHook a =
  PLEHook
    RPermute
    (LeftTransformation a)
    (EchelonForm a)
  deriving Show

data PLREHook a =
  PLREHook
    RPermute
    (LeftTransformation a)
    (EchelonTransformation a)
    (EchelonForm a)
  deriving Show

data RREF a =
  RREF
    (EchelonTransformation a)
    (EchelonForm a)
  deriving Show

--------------------------------------------------------------------------------
-- NFData
--------------------------------------------------------------------------------

instance NFData a => NFData (PLEHook a) where
  rnf (PLEHook p l e) =
    seq (rnf p) $ seq (rnf l) $ seq (rnf e) ()

instance NFData a => NFData (PLREHook a) where
  rnf (PLREHook p l r e) =
    seq (rnf p) $ seq (rnf l) $ seq (rnf r) $ seq (rnf e) ()

instance NFData a => NFData (RREF a) where
  rnf (RREF t e) =
    seq (rnf t) $ seq (rnf e) ()

--------------------------------------------------------------------------------
-- IsMatrix
--------------------------------------------------------------------------------

instance AdditiveMonoid a => IsMatrix (PLEHook a) a where
  toMatrix (PLEHook _ _ e) = toMatrix e

instance AdditiveMonoid a => IsMatrix (PLREHook a) a where
  toMatrix (PLREHook _ _ _ e) = toMatrix e

instance AdditiveMonoid a => IsMatrix (RREF a) a where
  toMatrix (RREF _ e) = toMatrix e
