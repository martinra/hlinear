{-# LANGUAGE
    StandaloneDeriving
  #-}

module HLinear.PLE.Hook.EchelonTransformation.Column
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


import Math.Structure.Tasty ()
import Test.QuickCheck.Arbitrary ( Arbitrary
                                 , arbitrary
                                 , shrink
                                 )
import Test.QuickCheck.Modifiers ( NonNegative(..) )


data EchelonTransformationColumn a =
  EchelonTransformationColumn
    { offset :: Int
    , init :: Vector a
    }

-- lenght and offset

length :: EchelonTransformationColumn a -> Int
length (EchelonTransformationColumn o v) = V.length v + 1 + o

length' :: EchelonTransformationColumn a -> (Int,Int,Int)
length' (EchelonTransformationColumn o v) = (V.length v,1,o)

setLength :: Int -> EchelonTransformationColumn a
          -> EchelonTransformationColumn a
setLength n (EchelonTransformationColumn _ v)
  | o' < 0 = error "EchelonTransformationColumn.setLength: negative offset"
  | otherwise = EchelonTransformationColumn o' v
  where
    o' = n - 1 - V.length v

-- access and conversion

(!) :: Ring a
    => EchelonTransformationColumn a -> Int -> a
(!) (EchelonTransformationColumn o v) ix
  | ix < l    = v V.! ix
  | ix == l   = one
  | ix < o+l  = zero
  | otherwise = error "EchelonTransformationColumn.(!): out of range"
  where
    l = V.length v

toVector
  :: Ring a
  => EchelonTransformationColumn a -> Vector a
toVector (EchelonTransformationColumn o v) =
  v `V.snoc` one V.++ V.replicate (fromIntegral o) zero

-- Eq and Show instancs

deriving instance Show a => Show (EchelonTransformationColumn a)

instance Eq a => Eq (EchelonTransformationColumn a) where
  (EchelonTransformationColumn o v) == (EchelonTransformationColumn o' v') =
    o == o' && (`V.all` V.zip v v') (uncurry (==))

isIdentityColumn
  :: ( DecidableZero a, DecidableOne a )
  => EchelonTransformationColumn a -> Bool
isIdentityColumn (EchelonTransformationColumn _ v) = V.all isZero v

-- creation

identityColumn
  :: AdditiveMonoid a
  => Int -> Int -> EchelonTransformationColumn a
identityColumn n o
  | n <= o = error "identityColumn: to large offset"
  | otherwise = EchelonTransformationColumn o $
                V.replicate (n-o-1) zero

-- QuickCheck

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
    ++
    [ EchelonTransformationColumn s v'
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
