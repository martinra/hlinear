module HLinear.Hook.EchelonTransformation.Basic
where

import qualified Prelude as P
import HLinear.Utility.Prelude hiding ( one )

import qualified Data.Vector as V
import qualified Math.Structure as MS

import HLinear.Hook.EchelonTransformation.Column hiding ( isOne, one )
import HLinear.Hook.EchelonTransformation.Definition
import HLinear.Matrix.Definition ( Matrix(..), IsMatrix(..) )
import HLinear.Utility.RPermute
import qualified HLinear.Hook.EchelonTransformation.Column as ETC


--------------------------------------------------------------------------------
-- attributes
--------------------------------------------------------------------------------

nmbCols :: EchelonTransformation a -> Natural
nmbCols = fromIntegral . V.length . columns

minimizeSize :: ( DecidableZero a, DecidableOne a )
             => EchelonTransformation a -> EchelonTransformation a
minimizeSize (EchelonTransformation nrs cs) =
  if null cs'
  then EchelonTransformation 0 V.empty
  else EchelonTransformation nrs' cs'
  where
    cs' = V.dropWhile ETC.isOne cs
    nrs' = fromIntegral $ fromIntegral nrs - (V.length cs - V.length cs')

--------------------------------------------------------------------------------
-- Eq, Show, and NFData instances
--------------------------------------------------------------------------------

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

instance NFData a => NFData (EchelonTransformation a) where
  rnf (EchelonTransformation nrs cs) =
    seq (rnf nrs) $
    seq (V.map rnf cs) ()

--------------------------------------------------------------------------------
-- container
--------------------------------------------------------------------------------

instance Functor EchelonTransformation where
  fmap = fmapDefault

instance Foldable EchelonTransformation where
  foldMap = foldMapDefault

instance Traversable EchelonTransformation where
  traverse f (EchelonTransformation nrs rs) =
    EchelonTransformation nrs <$> traverse (traverse f) rs

--------------------------------------------------------------------------------
-- creation
--------------------------------------------------------------------------------

singleton :: Vector a -> EchelonTransformation a
singleton v = EchelonTransformation nrs $ V.singleton $
                EchelonTransformationColumn 0 v
  where
    nrs = fromIntegral $ 1 + V.length v

one :: Natural -> EchelonTransformation a
one nrs = EchelonTransformation nrs V.empty

--------------------------------------------------------------------------------
-- conversion
--------------------------------------------------------------------------------

instance Ring a => IsMatrix (EchelonTransformation a) a where
  toMatrix (EchelonTransformation nrs cs) =
    Matrix nrs nrs $
      V.generate nrsZ $ \ix ->
      V.generate nrsZ $ \jx ->
        case compare ix jx of
          LT -> maybe zero (!ix) $ cs V.!? (nrsZ-1-jx)
          EQ -> MS.one
          GT -> zero
    where
    nrsZ = fromIntegral nrs

--------------------------------------------------------------------------------
-- subtransformations
--------------------------------------------------------------------------------

splitAt :: Int -> EchelonTransformation a
        -> (EchelonTransformation a, EchelonTransformation a)
splitAt ix et@(EchelonTransformation nrs cs)
  | ix <= nrsZ - ncsZ = (one ixN, et)
  | ix >= nrsZ        = (et, one nrs)
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
