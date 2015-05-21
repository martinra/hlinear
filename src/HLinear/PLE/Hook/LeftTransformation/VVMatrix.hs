{-# LANGUAGE
    MultiParamTypeClasses
  #-}

module HLinear.PLE.Hook.LeftTransformation.VVMatrix
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import qualified Data.Vector as V
import Math.Structure
import HLinear.PLE.Hook.LeftTransformation
import HLinear.VVMatrix
import HLinear.VVMatrix.Definition


-- we define this instance only for the constructor VVMatrix _ _ _
-- this is for testing purposes mostly
instance    ( DivisionRing a, LeftModule a b )
         => MultiplicativeSemigroupLeftAction (LeftTransformation a) (VVMatrix b) where
  lt *. (VVMatrix nrs' ncs' rs') = VVMatrix nrs' ncs' $ V.map (lt*.) rs'

instance    (DivisionRing a, LeftModule a b)
         => MultiplicativeLeftAction (LeftTransformation a) (VVMatrix b)
