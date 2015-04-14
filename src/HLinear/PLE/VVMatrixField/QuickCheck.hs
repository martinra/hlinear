module HLinear.PLE.VVMatrixField.QuickCheck
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

import HLinear.PLE.VVMatrixField.LeftTransformation ( LeftTransformation(..) )


instance    (DecidableZero a, Arbitrary a)
         => Arbitrary (LeftTransformation a) where
  arbitrary = do
    nrs'@(NonNegative (Small nrs)) <- arbitrary 
    NonNegative (Small ncs) <- arbitrary `suchThat` (<nrs') 
    cs <- V.generateM ncs $ \jx ->
            (,) <$> arbitrary <*> V.replicateM (nrs-jx-1) arbitrary
    return $ LeftTransformation (fromIntegral nrs) cs

  shrink (LeftTransformation nrs cs) =
    [ LeftTransformation (nrs-1) $
      dropIx jx $ (`V.imap` cs)
                  (\jx' (a,c) -> if jx'>=jx then (a,c)
                                 else (a, dropIx (jx-jx'-1) c))
    | jx <- [0..V.length cs - 1]
    ]
    ++
    map (LeftTransformation nrs) (V.mapM shrinkColumn cs)
    where
    dropIx :: Int -> Vector a -> Vector a
    dropIx ix v = V.take ix v V.++ V.drop (ix+1) v

    shrinkColumn (a,c) =
      [ (a',c) | a' <- shrink a ]
      ++
      [ (a,V.modify (\c' -> VM.write c' ix e) c)
      | ix <- [0..V.length c - 1]
      , e <- shrink (c V.! ix)
      ]
