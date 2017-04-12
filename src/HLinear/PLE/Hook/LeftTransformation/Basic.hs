{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , StandaloneDeriving
  #-}

module HLinear.PLE.Hook.LeftTransformation.Basic
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Applicative ( (<$>) )
import Control.Arrow ( first )
import Control.DeepSeq ( NFData(..) )
import Data.Maybe
import Data.Vector ( Vector(..) )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural ( Natural )


import HLinear.Matrix ( Matrix(..), IsMatrix(..) )
import HLinear.PLE.Hook.LeftTransformation.Column
import qualified HLinear.PLE.Hook.LeftTransformation.Column as LTC
import HLinear.PLE.Hook.LeftTransformation.Definition
import HLinear.PLE.Hook.RPermute


nmbCols :: LeftTransformation a -> Natural
nmbCols = fromIntegral . V.length . columns

minimizeSize :: ( DecidableZero a, DecidableOne a )
             => LeftTransformation a -> LeftTransformation a
minimizeSize (LeftTransformation nrs cs) =
  if null cs'
  then LeftTransformation 0 V.empty
  else LeftTransformation nrs' cs'
  where
    cs' = V.dropWhile isIdentityLTColumn cs
    nrs' = fromIntegral $ fromIntegral nrs - (V.length cs - V.length cs')

-- Eq, Show, and NFData instances

deriving instance Show a => Show (LeftTransformation a)

instance    ( Eq a, DecidableZero a, DecidableOne a )
         => Eq (LeftTransformation a) where
  -- this is equality in the injective limit of left transformations
  -- with respect to adding identity matrices to the top left
  lt == lt' =
    let LeftTransformation nrs cs = minimizeSize lt
        LeftTransformation nrs' cs' = minimizeSize lt'
        ncs = V.length cs
        ncs' = V.length cs'
    in nrs == nrs' && ncs == ncs'
       &&
       V.all (uncurry (==)) (V.zip cs cs')

instance NFData a => NFData (LeftTransformation a) where
  rnf (LeftTransformation nrs cs) = seq (rnf nrs) $ seq (rnf cs) ()

-- creation

identityLT :: Natural -> LeftTransformation a
identityLT nrs = LeftTransformation nrs V.empty

fromDiagonal :: Vector (NonZero a) -> LeftTransformation a
fromDiagonal ds = LeftTransformation nrs $ flip V.imap ds $ \ix d ->
                    LeftTransformationColumn ix d V.empty
  where
    nrsZ = V.length ds 
    nrs = fromIntegral nrsZ

-- todo: rename singleton
fromVector' :: DecidableZero a
            => Vector a -> LeftTransformation a
fromVector' v = LeftTransformation nrs $ V.singleton $
                  LeftTransformationColumn 0 a c
  where
  nrs = fromIntegral $ V.length v
  a = nonZero $ V.head v
  c = V.tail v

--------------------------------------------------------------------------------
-- conversion
--------------------------------------------------------------------------------

instance Ring a => IsMatrix (LeftTransformation a) a where
  toMatrix (LeftTransformation nrs cs) =
    Matrix nrs nrs $
      V.generate nrs' $ \ix ->
      V.generate nrs' $ \jx ->
        let a = maybe one LTC.head $ cs V.!? jx
        in
        case compare ix jx of
          LT -> zero
          EQ -> a
          GT -> maybe zero ((*a) . (!ix)) $ cs V.!? jx
    where
    nrs' = fromIntegral nrs

-- subtransformations

splitAt :: Int -> LeftTransformation a
        -> (LeftTransformation a, LeftTransformation a)
splitAt ix lt@(LeftTransformation nrs cs)
  | ix >= ncs = (lt, identityLT nrs')
  | otherwise =
      let (csLeft, csRight) = V.splitAt ix cs
      in ( LeftTransformation nrs csLeft
         , LeftTransformation nrs' $ V.map (LTC.setLength nrs'Z) csRight
         )
  where
    ncs = V.length cs
    nrs'Z = max 0 $ min nrsZ $ nrsZ - ix
    nrsZ = fromIntegral nrs
    nrs' = fromIntegral nrs'Z

drop :: Int -> LeftTransformation a -> LeftTransformation a
drop ix (LeftTransformation nrs cs) =
  LeftTransformation nrs' $ V.drop ix cs
  where
    nrs' = fromIntegral $ fromIntegral nrs - max 0 ix
