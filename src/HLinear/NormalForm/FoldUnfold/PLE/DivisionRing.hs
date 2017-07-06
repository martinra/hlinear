module HLinear.NormalForm.FoldUnfold.PLE.DivisionRing
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import qualified Prelude as P

import Math.Structure
import Numeric.Natural ( Natural )
import qualified Data.Vector as V

import HLinear.Hook.PLEHook ( PLEHook(..) )
import HLinear.Matrix.Definition ( Matrix(..) )
import HLinear.NormalForm.FoldUnfold.Matrix ( splitOffTopLeft )
import HLinear.NormalForm.FoldUnfold.Pivot ( pivotNonZeroPermutation )
import HLinear.Utility.RPermute ( RPermute(..) )
import qualified HLinear.Hook.PLEHook.Basic as Hook
import qualified HLinear.Hook.EchelonForm as EF
import qualified HLinear.Hook.LeftTransformation as LT
import qualified HLinear.Utility.RPermute as RP


type HasPLE a = ( DivisionRing a, DecidableZero a, DecidableUnit a )


ple :: HasPLE a => Matrix a -> PLEHook a
ple m@(Matrix nrs ncs _) =
  case splitOffHook m of
    Nothing -> Hook.one nrs ncs
    Just (h,m') -> V.foldl (*) h $ V.unfoldr splitOffHook m'

splitOffHook
  :: HasPLE a
  => Matrix a -> Maybe (PLEHook a, Matrix a)
splitOffHook m@(Matrix nrs ncs rs)
  | nrs == 0 || ncs == 0 = Nothing
  | Just p <- pivotNonZeroPermutation m = Just $
      let Just ((pivot, pivotBottom), (pivotTail, bottomRight)) =
            splitOffTopLeft (p *. m)

          pivotRecip = recip $ Unit pivot
          pivotTail' = fmap (fromUnit pivotRecip *) pivotTail

          lt = LT.singleton pivotRecip $ fmap negate pivotBottom
          ef = EF.singletonLeadingOne nrs pivotTail'
          
          bottomRight' =
            (\f -> V.zipWith f pivotBottom bottomRight) $ \h t ->
              V.zipWith (\pv te -> te - h * pv) pivotTail' t
      in  ( PLEHook p lt ef
          , Matrix (nrs P.- 1) (ncs P.- 1) bottomRight'
          )
  | otherwise = Just
      ( Hook.one nrs ncs
      , Matrix nrs (ncs P.- 1) $ fmap V.tail rs
      )
