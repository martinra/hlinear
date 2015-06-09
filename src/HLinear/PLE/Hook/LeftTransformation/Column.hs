{-# LANGUAGE
    FlexibleContexts
  , MultiParamTypeClasses
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

import HLinear.PLE.Hook.ReversePermute
import HLinear.VVMatrix hiding ( (!), (!?) )
import HLinear.VVMatrix.Utils
import HLinear.VVMatrix.Definition ( VVMatrix(..) )


data LeftTransformationColumn a =
  LeftTransformationColumn Int (NonZero a) (Vector a)
  deriving Show

(!) :: LeftTransformationColumn a -> Int -> a
(!) (LeftTransformationColumn offset a vs) ix
  | ix < offset  = error "LeftTransformationColumn (!) out of range"
  | ix == offset = fromNonZero a
  | otherwise    = vs V.! (ix - offset - 1)


ltcShiftOffset :: Int -> LeftTransformationColumn a
              -> LeftTransformationColumn a
ltcShiftOffset shift (LeftTransformationColumn offset a vs) =
  LeftTransformationColumn (offset + shift) a vs

ltcHead :: LeftTransformationColumn a -> a  
ltcHead (LeftTransformationColumn _ a _) = fromNonZero a

ltcHeadNonZero :: LeftTransformationColumn a -> NonZero a
ltcHeadNonZero (LeftTransformationColumn _ a _) = a

ltcHeadRecip :: MultiplicativeGroup (NonZero a)
            => LeftTransformationColumn a -> a  
ltcHeadRecip (LeftTransformationColumn _ na _) = fromNonZero $ recip na

ltcTail :: LeftTransformationColumn a -> Vector a
ltcTail (LeftTransformationColumn _ _ v) = v

toVector :: AdditiveMonoid a => LeftTransformationColumn a -> Vector a
toVector (LeftTransformationColumn s a v) =
  V.replicate (fromIntegral s) zero
  V.++ fromNonZero a `V.cons` v

instance Eq a => Eq (LeftTransformationColumn a) where
  (LeftTransformationColumn s a v) == (LeftTransformationColumn s' a' v') =
    s == s' && a == a' && (`V.all` V.zip v v') (uncurry (==))

instance    ( DecidableZero a, Arbitrary a )
         => Arbitrary (LeftTransformationColumn a) where
  arbitrary = do
    NonNegative s <- arbitrary
    NonNegative nv <- arbitrary
    a <- arbitrary

    return . LeftTransformationColumn s a =<< V.replicateM nv arbitrary

  shrink (LeftTransformationColumn s a v) =
    [ LeftTransformationColumn s' a v
    | s' <- shrink s
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
                
      

instance MultiplicativeSemigroupLeftAction ReversePermute (LeftTransformationColumn a) where
  p *. (LeftTransformationColumn s a v)
    | sizeRP p > V.length v = error "to large permutation"
    | otherwise             = LeftTransformationColumn s a (p *. v)
