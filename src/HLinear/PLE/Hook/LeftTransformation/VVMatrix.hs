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
import HLinear.VVMatrix.Creation
import HLinear.VVMatrix.Definition


instance    ( DivisionRing a, LeftModule a b )
         => MultiplicativeSemigroupLeftAction
              (LeftTransformation a) (VVMatrix b)
  where
  -- todo: this is not what we want: zero rows are given by V.empty
  lt *. (VVMatrix nrs' ncs' rs') = VVMatrix nrs' ncs' (lt *. rs')
  lt *. (Zero nrs' ncs') = Zero nrs' ncs'
  lt@(LeftTransformation nrs _) *. (One nrsMay' a') = (.* a') $
    case nrsMay' of
      Just nrs' -> case compare nrs nrs' of
                     GT -> let nrsDiff = fromIntegral nrs - fromIntegral nrs'
                           in submatix nrsDiff nrsDiff nrs nrs $ toMatrix lt
                     EQ -> toMatrix lt
                     LT -> let nrsDiff = fromIntegral nrs' - fromIntegral nrs
                           in identityMatrix nrsDiff `mappend` toMatrix lt
      Nothing   -> error "LeftTransformation *. (One Nothing _ :: VVMatrix)"

instance    (DivisionRing a, LeftModule a b)
         => MultiplicativeLeftAction (LeftTransformation a) (VVMatrix b)
