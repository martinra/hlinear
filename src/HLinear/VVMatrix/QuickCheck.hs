module HLinear.VVMatrix.QuickCheck
where

import Data.List ( transpose )
import qualified Data.Vector as V
import Test.QuickCheck.Arbitrary ( Arbitrary
                                 , arbitrary
                                 , shrink
                                 )

import HLinear.VVMatrix.Basic ( fromLists
                              , toLists
                              )
import HLinear.VVMatrix.Definition ( VVMatrix(..) )


instance Arbitrary a => Arbitrary (VVMatrix a) where
  arbitrary = do
    nrs <- arbitrary
    ncs <- arbitrary
    rs <- V.replicateM nrs $ V.replicateM ncs arbitrary
    return $ VVMatrix nrs ncs rs

  shrink = map fromLists . transpose . map shrink . toLists
