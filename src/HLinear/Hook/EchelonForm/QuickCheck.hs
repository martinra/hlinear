module HLinear.Hook.EchelonForm.QuickCheck
where

import HLinear.Utility.Prelude
import qualified Prelude as P

import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State.Strict ( evalStateT )
import Test.QuickCheck.Arbitrary ( Arbitrary, arbitrary, shrink )
import Test.QuickCheck.Modifiers ( NonNegative(..), Positive(..), Small(..) )
import qualified Data.Vector as V

import HLinear.Hook.EchelonForm.Definition as EF
import HLinear.Hook.EchelonForm.Basic as EF
import HLinear.Hook.EchelonForm.Row as EFR


instance    ( Arbitrary a, Ring a, DecidableZero a )
         => Arbitrary (EchelonForm a) where
  arbitrary = do
    NonNegative (Small lrs) <- arbitrary
    NonNegative (Small nrsDiff) <- arbitrary
    NonNegative (Small ncs) <- arbitrary
    let rs = V.replicateM lrs $ do
               Positive (Small o) <- lift arbitrary
               o' <- min ncs <$> gets (+o)
               put o'
               if ncs == o'
                 then return $ EchelonFormRow o' V.empty
                 else do
                   r <- V.replicateM (ncs-o'-1) (lift arbitrary)
                   r1 <- lift arbitrary
                   return $ EchelonFormRow o' $ (if isZero r1 then one else r1) `V.cons` r

    EchelonForm (lrs+nrsDiff) ncs <$> evalStateT rs 0

  shrink e = shrinkRow e <> shrinkCol e <> shrinkEntry e 
    where
      shrinkRow (EchelonForm nrs ncs rs)
        | nrs <= 1 = []
        | otherwise = [left,right]
          where
            nrs' = nrs `P.div` 2
            left = EchelonForm nrs' ncs leftrs
            (EchelonForm _ _ leftrs,right) =
              EF.splitAt nrs' e

      shrinkCol (EchelonForm nrs ncs rs)
        | ncs <= 1 = []
        | otherwise = [ EchelonForm nrs ncs' left
                      , EchelonForm nrs ncs'' right
                      ]
          where
            ncs' = ncs `P.div` 2
            ncs'' = ncs - ncs'
            (left,right) = V.unzip $
                           fmap (EFR.splitAt ncs') rs

      shrinkEntry (EchelonForm nrs ncs rs) = fmap (EchelonForm nrs ncs) $ do
        ix <- [0..V.length rs - 1]
        let EchelonFormRow o r = rs V.! ix
        jx <- [0..V.length r - 1]
        e <- shrink $ r V.! jx
        return $ V.update rs $ V.singleton
                   (ix, EchelonFormRow o $ V.update r $ V.singleton (jx,e))
