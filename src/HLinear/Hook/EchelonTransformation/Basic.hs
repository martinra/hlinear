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
-- resizing
--------------------------------------------------------------------------------

fitSize :: Ring a => Int -> EchelonTransformation a -> EchelonTransformation a
fitSize n lt@(EchelonTransformation nrs cs)
  | nrs >= n = lt
  | otherwise =
      let szDiff = n-nrs
          cs' = fmap (ETC.adjustOffset (+szDiff)) cs
          cszero = V.generate szDiff (ETC.one n) 
      in  EchelonTransformation n $ cszero <> cs'

--------------------------------------------------------------------------------
-- rows and columns
--------------------------------------------------------------------------------

instance HasNmbRows (EchelonTransformation a) where
  nmbRows (EchelonTransformation nrs _ ) = nrs

instance HasNmbCols (EchelonTransformation a) where
  nmbCols = nmbRows

--------------------------------------------------------------------------------
-- attributes
--------------------------------------------------------------------------------

minimizeSize :: ( DecidableZero a, DecidableOne a )
             => EchelonTransformation a -> EchelonTransformation a
minimizeSize (EchelonTransformation nrs cs) =
  if null cs'
  then EchelonTransformation 0 V.empty
  else EchelonTransformation nrs' cs'
  where
    cs' = V.dropWhile ETC.isOne cs
    nrs' = nrs - (V.length cs - V.length cs')

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
    seq (fmap rnf cs) ()

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
singleton v =
  EchelonTransformation (1 + V.length v) $ V.singleton $
    EchelonTransformationColumn 0 v

one :: Int -> EchelonTransformation a
one nrs
  | nrs >= 0 = EchelonTransformation nrs V.empty
  | nrs < 0  = error "EchelonTransformation.one: negative size"

--------------------------------------------------------------------------------
-- conversion
--------------------------------------------------------------------------------

instance Ring a => IsMatrix (EchelonTransformation a) a where
  toMatrix (EchelonTransformation nrs cs) =
    Matrix nrs nrs $
      V.generate nrs $ \ix ->
      V.generate nrs $ \jx ->
        case compare ix jx of
          LT -> maybe zero (!ix) $ cs V.!? (nrs-1-jx)
          EQ -> MS.one
          GT -> zero

--------------------------------------------------------------------------------
-- subtransformations
--------------------------------------------------------------------------------

splitAt
  :: AdditiveMonoid a
  => Int -> EchelonTransformation a
  -> (EchelonTransformation a, EchelonTransformation a)
splitAt ix et@(EchelonTransformation nrs cs)
  | ix <= nrs - V.length cs = (one (max 0 ix), et)
  | ix >= nrs        =
      let czeros = V.generate (ix-nrs) $ ETC.one ix
          cs' = fmap (ETC.adjustOffset (+(ix-nrs))) cs
      in  ( EchelonTransformation ix (czeros <> cs')
          , one 0)
  | otherwise =
      let (csRight, csLeft) = V.splitAt (nrs-ix) cs
          csLeft' = fmap (ETC.adjustOffset (+(ix-nrs))) csLeft
      in ( EchelonTransformation ix csLeft'
         , EchelonTransformation nrs csRight
         )

drop :: Int -> EchelonTransformation a -> EchelonTransformation a
drop ix (EchelonTransformation nrs cs) =
  EchelonTransformation (nrs - max 0 ix) $ V.drop ix cs

tail :: EchelonTransformation a -> EchelonTransformation a
tail (EchelonTransformation nrs cs) =
  EchelonTransformation (pred nrs) $ V.tail cs
