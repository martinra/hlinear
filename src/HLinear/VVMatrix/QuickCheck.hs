module HLinear.VVMatrix.QuickCheck
where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Test.QuickCheck ( suchThat )
import Test.QuickCheck.Arbitrary ( Arbitrary
                                 , arbitrary
                                 , shrink
                                 )
import Test.QuickCheck.Gen ( frequency
                           , elements )
import Test.QuickCheck.Modifiers ( NonNegative(..)
                                 , Small(..)
                                 )

import HLinear.VVMatrix.Definition ( VVMatrix(..) )


instance Arbitrary a => Arbitrary (VVMatrix a) where
  arbitrary = frequency [(1,arbZeroOne),(100,arbVV)]
    where
      arbZeroOne = elements [Zero, One]
      arbVV = do
        NonNegative (Small nrs) <- arbitrary
        NonNegative (Small ncs) <- arbitrary
        rs <- V.replicateM nrs $ V.replicateM ncs arbitrary
        return $ VVMatrix nrs ncs rs

  shrink Zero = []
  shrink One = []
  shrink (VVMatrix nrs ncs rs) =
      map (VVMatrix (nrs-1) ncs) (shrinkVec rs)
    ++ map (VVMatrix nrs (ncs-1)) (V.mapM shrinkVec rs)
    ++ map (VVMatrix nrs ncs)    (V.mapM (V.mapM shrink) rs)
    where
    shrinkVec v
      | l == 0 = []
      | otherwise = V.tail v:
                    [ V.slice 0 ix v V.++ V.slice (ix+1) (l-ix-1) v
                    | ix <- [1..l-1] ]
      where
        l = V.length v
    shrinkVecEntry vs = [ V.modify (\ws -> VM.write ws ix w) vs
                        | ix <- [0..V.length vs - 1]
                        , w <- shrinkEntry (vs V.! ix)
                        ]
    shrinkEntry v = [ V.modify (\w -> VM.write w ix a) v
                    | ix <- [0..V.length v -1]
                    , a <- shrink (v V.! ix)
                    ]

