module HLinear.PLE.Hook.LeftTransformation.QuickCheck
where

import Control.Arrow ( (&&&) )
import Data.Traversable ( forM )
import qualified Data.Vector as V
import Data.Vector ( Vector )
import qualified Data.Vector.Mutable as VM
import Math.Structure ( DecidableZero, NonZero )

import Math.Structure.Tasty ()
import Test.QuickCheck ( suchThat, Gen )
import Test.QuickCheck.Arbitrary ( Arbitrary
                                 , arbitrary
                                 , shrink
                                 )
import Test.QuickCheck.Modifiers ( NonNegative(..)
                                 , Small(..)
                                 )

import HLinear.PLE.Hook.LeftTransformation.Basic
import HLinear.PLE.Hook.LeftTransformation.Column

import Debug.Trace


instance    (DecidableZero a, Arbitrary a)
         => Arbitrary (LeftTransformation a) where
  arbitrary = do
    -- We use this slightly odd construction of nrs to avoid infinite loops
    -- that QuickCheck sometimes produces on using
    --   ncs <- arbitary `suchThat` (<nrs)
    NonNegative ncs <- arbitrary
    NonNegative nrsDiff <- arbitrary
    let nrs = ncs + nrsDiff

    cs <- V.generateM ncs $ \jx -> do
            a <- arbitrary
            c <- V.replicateM (nrs-jx-1) arbitrary
            return $ LeftTransformationColumn jx a c
              
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
