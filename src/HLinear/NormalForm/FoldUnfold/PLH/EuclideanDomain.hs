{-# LANGUAGE
    FlexibleContexts
  , MultiParamTypeClasses
  #-}

module HLinear.NormalForm.FoldUnfold.PLH.EuclideanDomain
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import qualified Prelude as P

import Data.Maybe
import Math.Structure
import Numeric.Natural ( Natural )
import qualified Data.Vector as V

import HLinear.Hook.PLEHook ( PLEHook(..) )
import HLinear.Matrix ( Matrix(..) )
import HLinear.NormalForm.FoldUnfold.Matrix ( splitOffTopLeft )
import HLinear.NormalForm.FoldUnfold.PLH.Normalization ( HasPLHNormalization(..) )
import HLinear.Utility.RPermute ( RPermute(..) )
import qualified HLinear.Hook.PLEHook.Basic as Hook
import qualified HLinear.Hook.EchelonForm as EF
import qualified HLinear.Hook.LeftTransformation as LT
import qualified HLinear.Utility.RPermute as RP


plh
  :: ( EuclideanDomain a, DecidableZero a, MultiplicativeGroup (Unit a)
     , HasPLHNormalization a )
  => Matrix a -> PLEHook a
plh m@(Matrix nrs ncs _) =
  case splitOffHook m of
    Nothing -> Hook.one nrs ncs
    Just (h,m') -> V.foldl (*) h $ V.unfoldr splitOffHook m'

pivotPermutation
  :: EuclideanDomain a
  => Matrix a -> Maybe RPermute
pivotPermutation (Matrix nrs ncs rs) =
  let v = V.map fromJust $ V.filter isJust $
            V.map (euclNorm . V.head) rs
  in  if V.null v then Nothing
      else Just $ RP.fromTransposition (fromIntegral nrs) (0,V.minIndex v)

splitOffHook
  :: ( EuclideanDomain a, DecidableZero a, HasPLHNormalization a )
  => Matrix a -> Maybe (PLEHook a, Matrix a)
splitOffHook m@(Matrix nrs ncs rs)
  | nrs == 0 || ncs == 0 = Nothing
  | Just p <- pivotPermutation m = Just $
      let Just ((pivot, pivotBottom), (pivotTail, bottomRight)) =
            splitOffTopLeft (p *. m)

          (pivotNormalization, pivot') = plhNormalization pivot
          pivotTail' = V.map (fromUnit pivotNormalization *) pivotTail

          (pivotBottomNormalization, pivotBottom') =
            V.unzip $ V.map (`quotRem` pivot') pivotBottom
          lt = LT.singleton pivotNormalization $
                 V.map negate pivotBottomNormalization
          ef = EF.singletonLeadingOne nrs pivotTail'
          
          bottomRight' =
            (\f -> V.zipWith f pivotBottomNormalization bottomRight) $ \h t ->
              V.zipWith (\pv te -> te - h * pv) pivotTail' t
      in  if V.all isZero pivotBottom
          then ( PLEHook p lt ef
               , Matrix (nrs P.- 1) (ncs P.- 1) bottomRight' 
               )
          else ( PLEHook p lt (EF.zero nrs ncs)
               , Matrix nrs ncs $
                   (pivot' `V.cons` pivotTail')
                   `V.cons`
                   (V.zipWith V.cons pivotBottom' bottomRight')
               )
  | otherwise = Just
      ( Hook.one nrs ncs
      , Matrix nrs (ncs P.- 1) $ V.map V.tail rs
      )
