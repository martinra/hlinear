{-# LANGUAGE
    ScopedTypeVariables
  #-}

module HLinear.BRMatrix.QuickCheck
where

import Control.Applicative ( (<$>) )
import Data.Proxy ( Proxy(..) )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import GHC.TypeLits ( Nat, KnownNat, natVal )

import Test.Natural ()
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

import HLinear.BRMatrix.RVector ( RVector(..) )
import qualified HLinear.BRMatrix.RVector as RV
import HLinear.BRMatrix.Definition ( BRMatrix(..) )


instance Arbitrary a => Arbitrary (BRMatrix a) where
  arbitrary = do
    NonNegative (Small nrs) <- arbitrary
    NonNegative (Small ncs) <- arbitrary
    rs <- fmap RVector $ V.replicateM nrs $
          fmap RVector $ V.replicateM ncs arbitrary
    return $ BRMatrix (fromIntegral nrs) (fromIntegral ncs) rs

  shrink (BRMatrix nrs ncs rs) =
    map ($rs)
    [ BRMatrix nrsD2 ncs .
       RV.liftRV (V.map (RV.liftRV $ V.take $ fromIntegral nrsD2))
    , BRMatrix nrsR2 ncs .
       RV.liftRV (V.map (RV.liftRV $ V.drop $ fromIntegral nrsD2))
    , BRMatrix nrs ncsD2 . RV.liftRV (V.take $ fromIntegral ncsD2)
    , BRMatrix nrs ncsR2 . RV.liftRV (V.drop $ fromIntegral ncsD2)
    ]
    ++
    [ ($rs) $ BRMatrix nrs ncs .
        RV.liftRV ( flip V.update $ V.singleton
                    ( ix, ($ toCurrentVector rs V.! ix) $
                          RV.liftRV $ flip V.update $ V.singleton
                          (jx, e)
                    )
                )
    | ix <- [0..fromIntegral nrs-1]
    , jx <- [0..fromIntegral ncs-1]
    , e  <- shrink $ toCurrentVector (toCurrentVector rs V.! ix) V.! jx
    ]
    where
      nrsD2 = nrs `div` 2
      nrsR2 = nrs - nrsD2
      ncsD2 = ncs `div` 2
      ncsR2 = ncs - ncsD2
