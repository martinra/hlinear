module HLinear.PLE.Hook.EchelonForm.Algebra
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import qualified Data.Vector as V
import Data.Vector ( Vector(..) )
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.Matrix.Definition ( Matrix(..) )
import HLinear.PLE.Hook.EchelonForm.Basic as EF
import HLinear.PLE.Hook.EchelonForm.Definition
import HLinear.PLE.Hook.EchelonForm.Row as EFR


--------------------------------------------------------------------------------
-- This can lead to inconsitent data structures. We assume that
-- the number of columns in the first argument equals the number of columns in
-- the second one + the offset of the first argument
--------------------------------------------------------------------------------

instance AdditiveMagma a => AdditiveMagma (EchelonForm a) where
  e@(EchelonForm nrs ncs rs) + e'@(EchelonForm nrs' ncs' rs')
    | lrs == 0 = ef (extendRows maxnrsZ minbrs nrs'Z brs' srs')
    | lrs' == 0 = ef (extendRows maxnrsZ minbrs nrsZ brs srs)
    -- this is the case that occur in the PLE decomposition
    | brs >= nrs'Z =
        let zeros = V.replicate (brs - nrs'Z) (EFR.zero maxncs)
        in  ef $ srs V.++ zeros V.++ srs'
    | otherwise = ef $ V.zipWith (+)
        (extendRows maxnrsZ minbrs nrsZ brs srs)
        (extendRows maxnrsZ minbrs nrs'Z brs' srs')
    where
      lrs = V.length rs
      lrs' = V.length rs'
      brs = nrsZ - lrs
      brs' = nrs'Z - lrs'

      maxnrs = max nrs nrs'
      maxncs = max ncs ncs'
      minbrs = min brs brs'

      maxnrsZ = fromIntegral maxnrs
      maxncsZ = fromIntegral maxncs
      nrsZ = fromIntegral nrs
      nrs'Z = fromIntegral nrs'

      srs = V.map (EFR.setLength maxncsZ) rs
      srs' = V.map (EFR.setLength maxncsZ) rs'

      ef = EchelonForm maxnrs maxncs

      extendRows mt mb t b rs =
         V.replicate (mt - t) (EFR.zero maxncs)
         V.++ rs V.++
         V.replicate (b - mb) (EFR.zero maxncs)

instance AdditiveSemigroup a => AdditiveSemigroup (EchelonForm a)

instance Abelian a => Abelian (EchelonForm a)
