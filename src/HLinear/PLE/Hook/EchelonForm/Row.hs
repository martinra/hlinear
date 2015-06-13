module HLinear.PLE.Hook.EchelonForm.Row
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import Control.Arrow ( first )
import qualified Data.Vector as V
import Data.Vector ( Vector(..) )
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.Matrix.Basic ()
import HLinear.Matrix.Definition ( Matrix(..) )

import Debug.Trace


-- | A vector of rows, each set off by a number of zeros
data EchelonFormRow a =
  EchelonFormRow
    { offset :: Natural
    , row :: Vector a
    }
  deriving Show

-- Eq

instance ( Eq a, DecidableZero a ) => Eq (EchelonFormRow a) where
  (EchelonFormRow o r) == (EchelonFormRow o' r') =
    V.all isZero (left V.++ left') && right == right'
    where
      maxo = max o o'
      (left,right, left',right') =
        case compare o o' of
          EQ -> (V.empty,r, V.empty,r')
          GT -> let (lf,rt) = V.splitAt (fromIntegral o - fromIntegral o') r'
                in (V.empty,r, lf,rt) 
          LT -> let (lf,rt) = V.splitAt (fromIntegral o' - fromIntegral o) r
                in (lf,rt, V.empty,r') 

-- length

length :: EchelonFormRow a -> Int
length (EchelonFormRow o r) = fromIntegral o + V.length r

setLength :: Int -> EchelonFormRow a -> EchelonFormRow a

setLength nrs (EchelonFormRow o r)
  | o' < 0 = error "EchelonFormRow.setLength: to long row"
  | otherwise = EchelonFormRow (fromIntegral o') r
  where
    o' = nrs - V.length r
  
-- creation

toVector :: AdditiveMonoid a => EchelonFormRow a -> Vector a
toVector (EchelonFormRow o r) = V.replicate (fromIntegral o) zero V.++ r

-- subrows

splitAt :: Int -> EchelonFormRow a -> (EchelonFormRow a, EchelonFormRow a)
splitAt ix (EchelonFormRow o r) =
  ( EchelonFormRow lefto leftr, EchelonFormRow righto rightr )
  where
    (lefto,righto) =
      if ix >= oZ
      then (o,0)
      else (fromIntegral ix0, fromIntegral $ oZ - ix0)
    ix0 = max 0 ix
    oZ = fromIntegral o
    (leftr,rightr) = V.splitAt (ix-oZ) r

-- additive structure

instance AdditiveMagma a => AdditiveMagma (EchelonFormRow a) where
  (EchelonFormRow o r) + (EchelonFormRow o' r') =
    EchelonFormRow (fromIntegral $ maxnr - V.length mr) mr
    where
      lr = V.length r
      lr' = V.length r'
      minlr = min lr lr'
      maxnr = max (fromIntegral o + lr) (fromIntegral o' + lr')
      mr = case compare lr lr' of
             EQ -> V.zipWith (+) r r'
             GT -> let (left,right) = V.splitAt (lr-lr') r
                   in left V.++ V.zipWith (+) right r'
             LT -> let (left,right) = V.splitAt (lr'-lr) r'
                   in left V.++ V.zipWith (+) r right

instance AdditiveSemigroup a => AdditiveSemigroup (EchelonFormRow a)
