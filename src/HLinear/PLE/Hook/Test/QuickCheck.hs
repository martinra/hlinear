module HLinear.PLE.Hook.Test.QuickCheck
where

import qualified Data.Vector as V
import Data.Vector ( Vector )
import qualified Data.Vector.Mutable as VM
import Math.Structure ( DecidableZero )

import Math.Structure.Tasty ()
import Test.QuickCheck ( suchThat )
import Test.QuickCheck.Arbitrary ( Arbitrary
                                 , arbitrary
                                 , shrink
                                 )
import Test.QuickCheck.Modifiers ( NonNegative(..)
                                 , Small(..)
                                 )

import HLinear.PLE.Hook.LeftTransformation
  ( LeftTransformation(..)
  , LeftTransformationColumn(..)
  )


instance    (DecidableZero a, Arbitrary a)
         => Arbitrary (LeftTransformation a) where
  arbitrary = do
    nrs'@(NonNegative (Small nrs)) <- arbitrary 
    NonNegative (Small ncs) <- arbitrary `suchThat` (<nrs') 
    cs <- V.generateM ncs $ \jx ->
            LeftTransformationColumn jx <$>
              arbitrary <*>
              V.replicateM (nrs-jx-1) arbitrary
    return $ LeftTransformation (fromIntegral nrs) cs

  shrink (LeftTransformation nrs cs) =
    [ LeftTransformation (nrs-1) $
      dropIx jx $ (`V.imap` cs)
        ( \jx' (LeftTransformationColumn s a c) ->
          LeftTransformationColumn s a $
          if jx'>=jx then c else dropIx (jx-jx'-1) c
        )
    | jx <- [0..V.length cs - 1]
    ]
    ++
    map (LeftTransformation nrs) (V.mapM shrinkColumn cs)
      where
      dropIx :: Int -> Vector a -> Vector a
      dropIx ix v = v1 V.++ V.tail v2
        where (v1,v2) = V.splitAt ix v

      shrinkColumn (LeftTransformationColumn s a c) =
        [ LeftTransformationColumn s a' c | a' <- shrink a ]
        ++
        [ LeftTransformationColumn s a $ V.update c $ V.singleton (ix,e)
        | ix <- [0..V.length c - 1]
        , e <- shrink (c V.! ix)
        ]
