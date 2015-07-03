{-# LANGUAGE
    StandaloneDeriving
  #-}

module HLinear.PLE.Hook.EchelonTransformation.Basic
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Vector ( Vector(..) )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.PLE.Hook.EchelonTransformation.Column
import qualified HLinear.PLE.Hook.EchelonTransformation.Column as ETC
import HLinear.PLE.Hook.RPermute


-- | An echelon transformation preserves Echelon forms, and thus is of the form
--   1  v  v  v
--   0  1  v  v
--   0  0  1  v
--   0  0  0  1
--  EchelonTransformationColumns all start at the top, but their lengths vary,
--  which is expressed by their offset parameter.
data EchelonTransformation a =
  EchelonTransformation
    { nmbRows :: Natural
    , columns :: Vector (EchelonTransformationColumn a)
    }

nmbCols :: EchelonTransformation a -> Natural
nmbCols = fromIntegral . V.length . columns

minimizeSize :: ( DecidableZero a, DecidableOne a )
             => EchelonTransformation a -> EchelonTransformation a
minimizeSize (EchelonTransformation nrs cs) =
  if null cs'
  then EchelonTransformation 0 V.empty
  else EchelonTransformation nrs' cs'
  where
    cs' = V.dropWhile isIdentityColumn cs
    nrs' = fromIntegral $ fromIntegral nrs - (V.length cs - V.length cs')

-- Eq an Show instances

deriving instance Show a => Show (EchelonTransformation a)

instance    ( Eq a, DecidableZero a, DecidableOne a )
         => Eq (EchelonTransformation a) where
  -- this is equality in the injective limit of left transformations
  -- with respect to adding identity matrices to the top left
  lt == lt' =
    let EchelonTransformation nrs cs = minimizeSize lt
        EchelonTransformation nrs' cs' = minimizeSize lt'
        ncs = V.length cs
        ncs' = V.length cs'
    in nrs == nrs' && ncs == ncs'
       &&
       V.all (uncurry (==)) (V.zip cs cs')

-- creation

singleton :: Vector a -> EchelonTransformation a
singleton v = EchelonTransformation nrs $ V.singleton $
                EchelonTransformationColumn 0 v
  where
    nrs = fromIntegral $ 1 + V.length v

identityET :: Natural -> EchelonTransformation a
identityET nrs = EchelonTransformation nrs V.empty

-- subtransformations

splitAt :: Int -> EchelonTransformation a
        -> (EchelonTransformation a, EchelonTransformation a)
splitAt ix et@(EchelonTransformation nrs cs)
  | ix <= nrsZ - ncsZ = (identityET ixN, et)
  | ix >= nrsZ        = (et, identityET nrs)
  | otherwise =
      let (csRight, csLeft) = V.splitAt (ix-nrsZ+ncsZ) cs
      in ( EchelonTransformation ixN $ V.map (ETC.setLength ix) csLeft
         , EchelonTransformation nrs csRight
         )
  where
    ixN = fromIntegral $ max 0 ix
    nrsZ = fromIntegral nrs
    ncsZ = V.length cs

drop :: Int -> EchelonTransformation a -> EchelonTransformation a
drop ix (EchelonTransformation nrs cs) =
  EchelonTransformation nrs' $ V.drop ix cs
  where
    nrs' = fromIntegral $ fromIntegral nrs - max 0 ix

tail :: EchelonTransformation a -> EchelonTransformation a
tail (EchelonTransformation nrs cs) =
  EchelonTransformation (pred nrs) $ V.tail cs
