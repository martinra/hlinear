module HLinear.Hook.EchelonTransformation.Column
where

import HLinear.Utility.Prelude hiding ( one, isOne )
import qualified Prelude as P

import Math.Structure.Tasty ()
import Test.QuickCheck.Arbitrary ( Arbitrary, arbitrary, shrink )
import Test.QuickCheck.Modifiers ( NonNegative(..) )
import qualified Data.Vector as V
import qualified Math.Structure as MS


data EchelonTransformationColumn a =
  EchelonTransformationColumn
    { offset :: !Int
    , init :: !(Vector a)
    }

--------------------------------------------------------------------------------
-- attributes
--------------------------------------------------------------------------------

length :: EchelonTransformationColumn a -> Int
length (EchelonTransformationColumn o v) = V.length v + 1 + o

length' :: EchelonTransformationColumn a -> (Int,Int,Int)
length' (EchelonTransformationColumn o v) = (V.length v,1,o)

adjustOffset
  :: (Int -> Int) -> EchelonTransformationColumn a
  -> EchelonTransformationColumn a
adjustOffset f (EchelonTransformationColumn o v) =
  EchelonTransformationColumn (f o) v

--------------------------------------------------------------------------------
-- container
--------------------------------------------------------------------------------

instance Functor EchelonTransformationColumn where
  fmap = fmapDefault

instance Foldable EchelonTransformationColumn where
  foldMap = foldMapDefault

instance Traversable EchelonTransformationColumn where
  traverse f (EchelonTransformationColumn o v) =
    EchelonTransformationColumn o <$> traverse f v

--------------------------------------------------------------------------------
-- access and conversion
--------------------------------------------------------------------------------

(!) :: Ring a
    => EchelonTransformationColumn a -> Int -> a
(!) (EchelonTransformationColumn o v) ix
  | ix < l    = v V.! ix
  | ix == l   = MS.one
  | ix < o+l  = zero
  | otherwise = error "EchelonTransformationColumn.(!): out of range"
  where
    l = V.length v

toVector
  :: Ring a
  => EchelonTransformationColumn a -> Vector a
toVector (EchelonTransformationColumn o v) =
  v `V.snoc` MS.one <> V.replicate o zero

--------------------------------------------------------------------------------
-- Eq, Show, and NFData instancs
--------------------------------------------------------------------------------

deriving instance Show a => Show (EchelonTransformationColumn a)

instance Eq a => Eq (EchelonTransformationColumn a) where
  (EchelonTransformationColumn o v) == (EchelonTransformationColumn o' v') =
    o == o' && (`V.all` V.zip v v') (uncurry (==))

instance NFData a => NFData (EchelonTransformationColumn a) where
  rnf (EchelonTransformationColumn o v) =
    seq (rnf o) $
    seq (fmap rnf v) ()

isOne
  :: ( DecidableZero a, DecidableOne a )
  => EchelonTransformationColumn a -> Bool
isOne = all isZero

--------------------------------------------------------------------------------
-- creation
--------------------------------------------------------------------------------

one
  :: AdditiveMonoid a
  => Int -> Int -> EchelonTransformationColumn a
one n o
  | n <= o = error "EchelonTransformationColumn.one: to large offset"
  | otherwise = EchelonTransformationColumn o $
                V.replicate (n-o-1) zero

--------------------------------------------------------------------------------
-- QuickCheck
--------------------------------------------------------------------------------

instance    ( DecidableZero a, Arbitrary a )
         => Arbitrary (EchelonTransformationColumn a) where
  arbitrary = do
    NonNegative o <- arbitrary
    NonNegative nv <- arbitrary

    return . EchelonTransformationColumn o =<< V.replicateM nv arbitrary

  shrink (EchelonTransformationColumn s v) =
    [ EchelonTransformationColumn s' v
    | s' <- shrink s, s' >= 0
    ]
    <>
    [ EchelonTransformationColumn s v'
    | v' <- shrinkVector v
    ]
      where
      shrinkVector v 
         | V.length v <= 1 = []
         | otherwise = 
           let (v1,v2) = V.splitAt (V.length v `P.div` 2) v
           in
           [v1,v2]
           <>
           [ V.update v $ V.singleton (ix,e) 
           | ix <- [0..V.length v-1]
           , e <- shrink (v V.! ix)
           ]
