module HLinear.PLE.Hook.EchelonForm.QuickCheck
where

import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State.Strict ( evalStateT )
import Control.Monad.State ( gets, put )
import qualified Data.Vector as V
import Data.Vector ( Vector )

import Math.Structure ( DecidableZero, Ring, isZero, one )

import Test.QuickCheck.Arbitrary ( Arbitrary
                                 , arbitrary
                                 , shrink
                                 )
import Test.QuickCheck.Modifiers ( NonNegative(..), Positive(..)
                                 , Small(..)
                                 )

import HLinear.PLE.Hook.EchelonForm.Definition as EF
import HLinear.PLE.Hook.EchelonForm.Basic as EF
import HLinear.PLE.Hook.EchelonForm.Row as EFR


instance    ( Arbitrary a, Ring a, DecidableZero a )
         => Arbitrary (EchelonForm a) where
  arbitrary = do
    NonNegative (Small lrsZ) <- arbitrary
    NonNegative (Small nrsDiffZ) <- arbitrary
    NonNegative (Small ncsZ) <- arbitrary
    let lrs = fromIntegral (lrsZ :: Integer)
    let nrsDiff = fromIntegral (nrsDiffZ :: Integer)
    let ncs = fromIntegral (ncsZ :: Integer)
    let rs = V.replicateM (fromIntegral lrs) $ do
               Positive (Small oZ) <- lift arbitrary
               o' <- min ncs <$> gets (+ fromInteger oZ)
               put o'
               if ncs == o'
                 then return $ EchelonFormRow o' V.empty
                 else do
                   r <- V.replicateM (fromIntegral ncs - fromIntegral o' - 1)
                                     (lift arbitrary)
                   r1 <- lift arbitrary
                   return $ EchelonFormRow o' $ (if isZero r1 then one else r1) `V.cons` r

    EchelonForm (lrs+nrsDiff) ncs <$> evalStateT rs 0

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
