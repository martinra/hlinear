{-# LANGUAGE UndecidableInstances #-}

module HLinear.Hook.LeftTransformation.Column
where

import qualified Prelude as P
import HLinear.Utility.Prelude hiding ( one, isOne )

import Math.Structure.Tasty ()
import Test.QuickCheck.Arbitrary ( Arbitrary, arbitrary, shrink )
import Test.QuickCheck.Modifiers ( NonNegative(..) )
import Test.SmallCheck.Series (Serial, series, decDepth )
import Test.Vector ()
import qualified Data.Vector as V
import qualified Math.Structure as MS

import HLinear.Matrix.Column ( Column(..) )
import HLinear.Utility.RPermute ( RPermute )
import qualified HLinear.Utility.RPermute as RP


data LeftTransformationColumn a =
  LeftTransformationColumn
    { offset :: Int
    , headUnit :: Unit a
    , tail :: Vector a
    }

--------------------------------------------------------------------------------
-- attributes
--------------------------------------------------------------------------------

length :: LeftTransformationColumn a -> Int
length (LeftTransformationColumn s _ v) = s + 1 + V.length v

length' :: LeftTransformationColumn a -> (Int,Int,Int)
length' (LeftTransformationColumn s _ v) = (s,1,V.length v)

adjustOffset
  :: (Int -> Int)
  -> LeftTransformationColumn a
  -> LeftTransformationColumn a
adjustOffset f (LeftTransformationColumn o a v) =
  LeftTransformationColumn (f o) a v

--------------------------------------------------------------------------------
-- container
--------------------------------------------------------------------------------

instance Functor LeftTransformationColumn where
  fmap = fmapDefault

instance Foldable LeftTransformationColumn where
  foldMap = foldMapDefault

instance Traversable LeftTransformationColumn where
  traverse f (LeftTransformationColumn o (Unit a) v) =
    LeftTransformationColumn o <$> fmap Unit (f a) <*> traverse f v

--------------------------------------------------------------------------------
-- access and conversion
--------------------------------------------------------------------------------

(!) ::
     AdditiveMonoid a
  => LeftTransformationColumn a -> Int -> a
(!) (LeftTransformationColumn o a v) ix
  | ix < o    = zero
  | ix == o   = fromUnit a
  | otherwise = v V.! (ix - o - 1)

head :: LeftTransformationColumn a -> a  
head = fromUnit . headUnit

headRecip :: MultiplicativeGroup (Unit a)
  => LeftTransformationColumn a -> a  
headRecip = fromUnit . recip . headUnit

toVector :: Rng a
  => LeftTransformationColumn a -> Vector a
toVector (LeftTransformationColumn o a v) =
  V.replicate o zero
  <>
  a' `V.cons` fmap (*a') v
  where
    a' = fromUnit a

toColumn :: Rng a
  => LeftTransformationColumn a -> Column a
toColumn = Column . toVector

--------------------------------------------------------------------------------
-- Eq, Show, and NFData instancs
--------------------------------------------------------------------------------

deriving instance Show a => Show (LeftTransformationColumn a)

instance Eq a => Eq (LeftTransformationColumn a) where
  (LeftTransformationColumn s a v) == (LeftTransformationColumn s' a' v') =
    s == s' && a == a' && (`V.all` V.zip v v') (uncurry (==))

isOne ::
     ( DecidableZero a, DecidableOne a )
  => LeftTransformationColumn a -> Bool
isOne (LeftTransformationColumn _ a v) =
  MS.isOne (fromUnit a) && V.all isZero v

instance NFData a => NFData (LeftTransformationColumn a) where
  rnf (LeftTransformationColumn s (Unit a) c) =
    seq (rnf s) $ seq (rnf a) $ seq (rnf c) ()

--------------------------------------------------------------------------------
-- creation
--------------------------------------------------------------------------------

one ::
     Ring a
  => Int -> Int -> LeftTransformationColumn a
one n o
  | n <= o = error "identityLTColumn: to large offset"
  | otherwise = LeftTransformationColumn o MS.one $
                  V.replicate (n-o-1) zero

--------------------------------------------------------------------------------
-- permutation action
--------------------------------------------------------------------------------

instance MultiplicativeSemigroupLeftAction RPermute (LeftTransformationColumn a) where
  p *. (LeftTransformationColumn s a v)
    | RP.size p > V.length v =
        error "RPermute *. LeftTransformationColumn: permutation too large"
    | otherwise = LeftTransformationColumn s a $ p *. v

--------------------------------------------------------------------------------
-- QuickCheck
--------------------------------------------------------------------------------

instance    ( Arbitrary a, Arbitrary (Unit a) )
         => Arbitrary (LeftTransformationColumn a) where
  arbitrary = do
    NonNegative s <- arbitrary
    NonNegative nv <- arbitrary
    a <- arbitrary
    cs <- V.replicateM nv arbitrary
    return $ LeftTransformationColumn s a cs

  shrink (LeftTransformationColumn s a v) =
    [ LeftTransformationColumn s' a v
    | s' <- shrink s, s' >= 0
    ]
    <>
    [ LeftTransformationColumn s a' v
    | a' <- shrink a
    ]
    <>
    [ LeftTransformationColumn s a v'
    | v' <- shrink v
    ]

--------------------------------------------------------------------------------
-- SmallCheck
--------------------------------------------------------------------------------

instance
     (Monad m, Serial m a, Serial m (Unit a))
  => Serial m (LeftTransformationColumn a)
  where
    series = do
      s <- series
      guard $ s >= 0
      nv <- series
      guard $ nv >= 0
      a <- series
      cs <- V.replicateM nv series
      return $ LeftTransformationColumn s a cs
