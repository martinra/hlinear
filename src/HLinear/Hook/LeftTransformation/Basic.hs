{-# LANGUAGE ScopedTypeVariables #-}

module HLinear.Hook.LeftTransformation.Basic
where

import qualified Prelude as P
import HLinear.Utility.Prelude hiding ( one )

import qualified Data.Vector as V
import qualified Math.Structure as MS


import HLinear.Matrix.Definition ( Matrix(..), IsMatrix(..) )
import qualified HLinear.Matrix.Basic as M
import HLinear.Matrix.Block ()
import HLinear.Hook.LeftTransformation.Column hiding ( one, isOne )
import qualified HLinear.Hook.LeftTransformation.Column as LTC
import HLinear.Hook.LeftTransformation.Definition
import HLinear.Utility.RPermute

--------------------------------------------------------------------------------
-- resizing
--------------------------------------------------------------------------------

minimizeSize :: ( DecidableZero a, DecidableOne a )
             => LeftTransformation a -> LeftTransformation a
minimizeSize (LeftTransformation nrs cs) =
  if null cs'
  then LeftTransformation 0 V.empty
  else LeftTransformation nrs' cs'
  where
    cs' = V.dropWhile LTC.isOne cs
    nrs' = fromIntegral $ fromIntegral nrs - (V.length cs - V.length cs')
minimizeSize lt@(LeftTransformationMatrix m) = lt

fitSize :: Ring a => Int -> LeftTransformation a -> LeftTransformation a
fitSize n lt@(LeftTransformation nrs cs)
  | nrsZ >= n = lt
  | otherwise =
      let szDiff = n-nrsZ
          cs' = fmap (LTC.adjustOffset (+szDiff)) cs
          cszero = V.generate szDiff (LTC.one n) 
      in  LeftTransformation (fromIntegral n) $ cszero <> cs'
  where
    nrsZ = fromIntegral nrs
fitSize n lt@(LeftTransformationMatrix m)
  | nrsZ >= n = lt
  | otherwise = LeftTransformationMatrix $ M.one (fromIntegral $ n-nrsZ) <> m
  where
    nrsZ = fromIntegral $ nmbRows m

--------------------------------------------------------------------------------
-- Eq, Show, and NFData instances
--------------------------------------------------------------------------------

deriving instance Show a => Show (LeftTransformation a)

instance    ( Eq a, Ring a, DecidableZero a, DecidableOne a )
         => Eq (LeftTransformation a) where
  -- this is equality in the injective limit of left transformations
  -- with respect to adding identity matrices to the top left
  lt@(LeftTransformation _ _) == lt'@(LeftTransformation _ _) =
    let LeftTransformation nrs  cs  = minimizeSize lt
        LeftTransformation nrs' cs' = minimizeSize lt'
        ncs = V.length cs
        ncs' = V.length cs'
    in    nrs == nrs'
       && V.and (V.zipWith (==) cs cs')
       && case compare ncs ncs' of
            EQ -> True
            GT -> V.all LTC.isOne $ V.drop ncs' cs
            LT -> V.all LTC.isOne $ V.drop ncs cs'
  lt == lt' =
    let nrsmax = fromIntegral $ max (nmbRows lt) (nmbRows lt')
        m = toMatrix (fitSize nrsmax lt) :: Matrix a
        m' = toMatrix (fitSize nrsmax lt') :: Matrix a
    in  m == m'

instance NFData a => NFData (LeftTransformation a) where
  rnf (LeftTransformation nrs cs) = seq (rnf nrs) $ seq (rnf cs) ()

--------------------------------------------------------------------------------
-- rows and columns
--------------------------------------------------------------------------------

instance HasNmbRows (LeftTransformation a) where
  nmbRows (LeftTransformationMatrix m) = nmbRows m
  nmbRows (LeftTransformation nrs _) = nrs

instance HasNmbCols (LeftTransformation a) where
  nmbCols (LeftTransformationMatrix m) = nmbCols m
  nmbCols (LeftTransformation nrs _) = nrs

--------------------------------------------------------------------------------
-- container
--------------------------------------------------------------------------------

instance Functor LeftTransformation where
  fmap = fmapDefault

instance Foldable LeftTransformation where
  foldMap = foldMapDefault

instance Traversable LeftTransformation where
  traverse f (LeftTransformation nrs rs) =
    LeftTransformation nrs <$> traverse (traverse f) rs

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
  toMatrix (LeftTransformationMatrix m) = m
  toMatrix (LeftTransformation nrs cs) =
    Matrix nrs nrs $
      V.generate nrsZ $ \ix ->
      V.generate nrsZ $ \jx ->
        let a = maybe MS.one LTC.head $ cs V.!? jx
        in
        case compare ix jx of
          LT -> zero
          EQ -> a
          GT -> maybe zero ((*a) . (!ix)) $ cs V.!? jx
    where
    nrsZ = fromIntegral nrs

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
         , LeftTransformation nrs' $ fmap (LTC.adjustOffset (+(nrs'Z-nrsZ))) csRight
         )
  where
    ncs = V.length cs
    nrs'Z = max 0 $ min nrsZ $ nrsZ - ix
    nrsZ = fromIntegral nrs
    nrs' = fromIntegral nrs'Z

drop :: Int -> LeftTransformation a -> LeftTransformation a
drop ix lt@(LeftTransformation nrs cs)
  | ix < 0 = lt
  | otherwise = LeftTransformation nrs' $
                  fmap (LTC.adjustOffset (subtract ix)) $ V.drop ix cs
  where
    nrs' = fromIntegral $ fromIntegral nrs - ix
