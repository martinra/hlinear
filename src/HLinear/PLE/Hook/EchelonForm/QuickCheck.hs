module HLinear.PLE.Hook.EchelonForm.QuickCheck
where

import qualified Data.Vector as V
import Data.Vector ( Vector )

import Test.QuickCheck.Arbitrary ( Arbitrary
                                 , arbitrary
                                 , shrink
                                 )
import Test.QuickCheck.Modifiers ( NonNegative(..)
                                 , Small(..)
                                 )

import HLinear.PLE.Hook.EchelonForm.Definition as EF
import HLinear.PLE.Hook.EchelonForm.Basic as EF
import HLinear.PLE.Hook.EchelonForm.Row as EFR

import Debug.Trace


instance Arbitrary a => Arbitrary (EchelonForm a) where
  arbitrary = do
    NonNegative (Small lrsZ) <- arbitrary
    NonNegative (Small nrsDiffZ) <- arbitrary
    NonNegative (Small ncsZ) <- arbitrary
    let lrs = fromIntegral (lrsZ :: Integer)
    let nrsDiff = fromIntegral (nrsDiffZ :: Integer)
    let ncs = fromIntegral (ncsZ :: Integer)
  
    EchelonForm (lrs+nrsDiff) ncs <$>
      ( V.replicateM (fromIntegral lrs) $ do
          NonNegative (Small oZ) <- arbitrary
          let o = fromIntegral (oZ :: Integer)
          let o' = min o ncs
          EchelonFormRow o' <$>
            V.replicateM (fromIntegral ncs - fromIntegral o') arbitrary
      )

  shrink e = shrinkRow e ++ shrinkCol e ++ shrinkEntry e 
    where
      shrinkRow (EchelonForm nrs ncs rs)
        | nrs <= 1 = []
        | otherwise = [left,right]
          where
            nrs' = nrs `div` 2
            left = EchelonForm nrs' ncs leftrs
            (EchelonForm _ _ leftrs,right) =
              EF.splitAt (fromIntegral nrs') e

      shrinkCol (EchelonForm nrs ncs rs)
        | ncs <= 1 = []
        | otherwise = [ EchelonForm nrs ncs' left
                      , EchelonForm nrs ncs'' right
                      ]
          where
            ncs' = ncs `div` 2
            ncs'' = fromIntegral $ fromIntegral ncs - fromIntegral ncs'
            (left,right) = V.unzip $
                           V.map (EFR.splitAt $ fromIntegral ncs') rs

      shrinkEntry (EchelonForm nrs ncs rs) = map (EchelonForm nrs ncs) $ do
        ix <- [0..V.length rs - 1]
        let EchelonFormRow o r = rs V.! ix
        jx <- [0..V.length r - 1]
        e <- shrink $ r V.! jx
        return $ V.update rs $ V.singleton
                   (ix, EchelonFormRow o $ V.update r $ V.singleton (jx,e))
    
