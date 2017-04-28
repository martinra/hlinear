{-# LANGUAGE
    FlexibleContexts
  , ScopedTypeVariables
  #-}

module HLinear.Test.Utility.Vector
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Vector ( Vector )
import Math.Structure
import Numeric.Natural ( Natural )
import qualified Data.Vector as V

import HLinear.Matrix ( Matrix(..), IsMatrix(..), Column(..) )


matrixActionOnTopVector
  :: forall a b
   . ( Eq b, IsMatrix a b, Rng b
     , MultiplicativeSemigroupLeftAction a (Column b) )
  => Natural -> a -> Column b -> Bool
matrixActionOnTopVector nrs a c@(Column v)
  | nv <= nrsZ =
      let v' = v `mappend` V.replicate (nrsZ-nv) zero
          m = toMatrix a :: Matrix b
      in  m *. Column v' == a *. c
  | otherwise =
      let (vt,vb) = V.splitAt nrsZ v
          m = toMatrix a :: Matrix b
      in  m *. Column vt `mappend` Column vb == a *. c
  where
    nv = V.length v
    nrsZ = fromIntegral nrs

matrixActionOnBottomVector
  :: forall a b
   . ( Eq b, IsMatrix a b, Rng b
     , MultiplicativeSemigroupLeftAction a (Column b) )
  => Natural -> a -> Column b -> Bool
matrixActionOnBottomVector nrs a c@(Column v)
  | nv <= nrsZ =
      let v' = V.replicate (nrsZ-nv) zero `mappend` v
          m = toMatrix a :: Matrix b
      in  m *. Column v' == a *. c
  | otherwise =
      let (vt,vb) = V.splitAt (nv-nrsZ) v
          m = toMatrix a :: Matrix b
      in  (Column vt) `mappend` (m *. Column vb) == a *. c
  where
    nv = V.length v
    nrsZ = fromIntegral nrs
