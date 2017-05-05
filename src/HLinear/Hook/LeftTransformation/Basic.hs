{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , StandaloneDeriving
  #-}

module HLinear.Hook.LeftTransformation.Basic
where

import qualified Prelude as P
import HLinear.Utility.Prelude hiding ( one )

import qualified Data.Vector as V
import qualified Math.Structure as MS


import HLinear.Matrix.Definition ( Matrix(..), IsMatrix(..) )
import HLinear.Hook.LeftTransformation.Column hiding ( one, isOne )
import qualified HLinear.Hook.LeftTransformation.Column as LTC
import HLinear.Hook.LeftTransformation.Definition
import HLinear.Utility.RPermute


nmbCols :: LeftTransformation a -> Natural
nmbCols = fromIntegral . V.length . columns

minimizeSize :: ( DecidableZero a, DecidableOne a )
             => LeftTransformation a -> LeftTransformation a
minimizeSize (LeftTransformation nrs cs) =
  if null cs'
  then LeftTransformation 0 V.empty
  else LeftTransformation nrs' cs'
  where
    cs' = V.dropWhile LTC.isOne cs
    nrs' = fromIntegral $ fromIntegral nrs - (V.length cs - V.length cs')

--------------------------------------------------------------------------------
-- Eq, Show, and NFData instances
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- rows and columns
--------------------------------------------------------------------------------

instance HasNmbRows (LeftTransformation a) where
  nmbRows (LeftTransformation nrs _) = nrs

instance HasNmbCols (LeftTransformation a) where
  nmbCols (LeftTransformation nrs _) = nrs

--------------------------------------------------------------------------------
-- creation
--------------------------------------------------------------------------------

one :: Natural -> LeftTransformation a
one nrs = LeftTransformation nrs V.empty

diagonal :: Vector (Unit a) -> LeftTransformation a
diagonal ds = LeftTransformation nrs $ flip V.imap ds $ \ix d ->
                    LeftTransformationColumn ix d V.empty
  where
    nrsZ = V.length ds 
    nrs = fromIntegral nrsZ

singleton ::
  Unit a -> Vector a -> LeftTransformation a
singleton a v =
  LeftTransformation (fromIntegral $ 1 + V.length v) $
    V.singleton $ LeftTransformationColumn 0 a v

singletonAdditive ::
     Rig a
  => Vector a -> LeftTransformation a
singletonAdditive = singleton MS.one

singletonMultiplicative ::
     (AdditiveMonoid a, DecidableZero a)
  => Unit a -> Natural -> LeftTransformation a
singletonMultiplicative a nrs_pred =
  singleton a $ V.replicate (fromIntegral nrs_pred) zero

fromVector ::
     DecidableUnit a
  => Vector a -> LeftTransformation a
fromVector v = singleton (toUnit $ V.head v) (V.tail v)

--------------------------------------------------------------------------------
-- conversion
--------------------------------------------------------------------------------

instance Ring a => IsMatrix (LeftTransformation a) a where
  toMatrix (LeftTransformation nrs cs) =
    Matrix nrs nrs $
      V.generate nrs' $ \ix ->
      V.generate nrs' $ \jx ->
        let a = maybe MS.one LTC.head $ cs V.!? jx
        in
        case compare ix jx of
          LT -> zero
          EQ -> a
          GT -> maybe zero ((*a) . (!ix)) $ cs V.!? jx
    where
    nrs' = fromIntegral nrs

--------------------------------------------------------------------------------
-- subtransformations
--------------------------------------------------------------------------------

splitAt :: Int -> LeftTransformation a
        -> (LeftTransformation a, LeftTransformation a)
splitAt ix lt@(LeftTransformation nrs cs)
  | ix >= ncs = (lt, one nrs')
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
