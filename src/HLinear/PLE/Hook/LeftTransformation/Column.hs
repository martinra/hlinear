{-# LANGUAGE
    FlexibleContexts
  , MultiParamTypeClasses
  , StandaloneDeriving
  #-}

module HLinear.PLE.Hook.LeftTransformation.Column
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Applicative ( (<$>) )
import Control.Arrow ( first )
import Data.Maybe
import Data.Vector ( Vector(..) )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural ( Natural )


import Math.Structure.Tasty ()
import Test.QuickCheck.Arbitrary ( Arbitrary
                                 , arbitrary
                                 , shrink
                                 )
import Test.QuickCheck.Modifiers ( NonNegative(..) )

import HLinear.PLE.Hook.RPermute as RP
import HLinear.Matrix.Algebra ( Column(..), unColumn )


data LeftTransformationColumn a =
  LeftTransformationColumn
    { offset :: Int
    , headNonZero :: NonZero a
    , tail :: Vector a
    }

-- lenght and offset

length :: LeftTransformationColumn a -> Int
length (LeftTransformationColumn s _ v) = s + 1 + V.length v

length' :: LeftTransformationColumn a -> (Int,Int,Int)
length' (LeftTransformationColumn s _ v) = (s,1,V.length v)

setLength :: Int -> LeftTransformationColumn a
          -> LeftTransformationColumn a
setLength n (LeftTransformationColumn _ a v)
  | o' < 0 = error "LeftTransformationColumn.setLength: to large offset"
  | otherwise = LeftTransformationColumn o' a v
  where
    o' = n - 1 - V.length v

shiftOffset :: Int -> LeftTransformationColumn a
            -> LeftTransformationColumn a
shiftOffset s (LeftTransformationColumn o a vs) =
  LeftTransformationColumn (o+s) a vs

-- access and conversion

(!) :: LeftTransformationColumn a -> Int -> a
(!) (LeftTransformationColumn o a v) ix
  | ix < o    = error "LeftTransformationColumn.(!): out of range"
  | ix == o   = fromNonZero a
  | otherwise = v V.! (ix - o - 1)

head :: LeftTransformationColumn a -> a  
head = fromNonZero . headNonZero

headRecip :: MultiplicativeGroup (NonZero a)
            => LeftTransformationColumn a -> a  
headRecip = fromNonZero . recip . headNonZero

toVector :: Rng a
         => LeftTransformationColumn a -> Vector a
toVector (LeftTransformationColumn o a v) =
  V.replicate (fromIntegral o) zero
  V.++ a' `V.cons` V.map (*a') v
  where
    a' = fromNonZero a

-- Eq and Show instancs

deriving instance Show a => Show (LeftTransformationColumn a)

instance Eq a => Eq (LeftTransformationColumn a) where
  (LeftTransformationColumn s a v) == (LeftTransformationColumn s' a' v') =
    s == s' && a == a' && (`V.all` V.zip v v') (uncurry (==))

-- creation

identityLTColumn :: ( Ring a, DecidableZero a )
                 => Int -> Int -> LeftTransformationColumn a
identityLTColumn n o | n <= o = error "identityLTColumn: to large offset"
                     | otherwise = LeftTransformationColumn o (nonZero one) $
                                     V.replicate (n-o-1) zero

-- QuickCheck

instance    ( DecidableZero a, Arbitrary a )
         => Arbitrary (LeftTransformationColumn a) where
  arbitrary = do
    NonNegative s <- arbitrary
    NonNegative nv <- arbitrary
    a <- arbitrary

    return . LeftTransformationColumn s a =<< V.replicateM nv arbitrary

  shrink (LeftTransformationColumn s a v) =
    [ LeftTransformationColumn s' a v
    | s' <- shrink s, s' >= 0
    ]
    ++
    [ LeftTransformationColumn s a' v
    | a' <- shrink a
    ]
    ++
    [ LeftTransformationColumn s a v'
    | v' <- shrinkVector v
    ]
      where
      shrinkVector v 
         | V.length v <= 1 = []
         | otherwise = 
           let (v1,v2) = V.splitAt (V.length v `div` 2) v
           in
           [v1,v2]
           ++
           [ V.update v $ V.singleton (ix,e) 
           | ix <- [0..V.length v-1]
           , e <- shrink (v V.! ix)
           ]
                
-- partially defined permutation action

instance MultiplicativeSemigroupLeftAction
           RPermute
           (LeftTransformationColumn a)
  where
  p *. (LeftTransformationColumn s a v)
    | RP.size p > V.length v =
        error "RPermute *. LeftTransformationColumn: permutation too large"
    | otherwise = LeftTransformationColumn s a $
                    RP.fromRPVector $ p *. RPVector v
