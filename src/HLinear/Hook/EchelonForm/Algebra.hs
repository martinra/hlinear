module HLinear.Hook.EchelonForm.Algebra
where

import HLinear.Utility.Prelude as P

import qualified Data.Vector as V
import qualified Math.Structure as MS

import HLinear.Hook.EchelonForm.Basic as EF
import HLinear.Hook.EchelonForm.Definition
import HLinear.Hook.EchelonForm.PivotStructure
import HLinear.Hook.EchelonForm.Row as EFR
import HLinear.Matrix.Definition ( Matrix(..) )


--------------------------------------------------------------------------------
-- This can lead to inconsitent data structures. We assume that
-- the number of columns in the first argument equals the number of columns in
-- the second one + the offset of the first argument
-- todo: we can obtain a consistend addition by sorting rows, if necessary
--------------------------------------------------------------------------------

instance AdditiveMagma a => AdditiveMagma (EchelonForm a) where
  {-# INLINABLE (+) #-}
  e@(EchelonForm nrs ncs rs) + e'@(EchelonForm nrs' ncs' rs')
    | lrs  == 0 = ef (extendRows maxnrs minbrs nrs' brs' srs')
    | lrs' == 0 = ef (extendRows maxnrs minbrs nrs brs srs)
    | otherwise = ef $ case compare brs nrs' of
       -- the first one is the case that occurs in the PLE decomposition
       EQ -> srs `mappend` srs'
       GT -> let zeros = V.replicate (brs - nrs') (EFR.zero maxncs)
             in  srs `mappend` zeros `mappend` srs'
       LT -> V.zipWith (+)
              (extendRows maxnrs minbrs nrs brs srs)
              (extendRows maxnrs minbrs nrs' brs' srs')
    where
      lrs = V.length rs
      lrs' = V.length rs'
      brs = nrs - lrs
      brs' = nrs' - lrs'

      maxnrs = max nrs nrs'
      maxncs = max ncs ncs'
      minbrs = min brs brs'

      srs = fmap (EFR.setLength maxncs) rs
      srs' = fmap (EFR.setLength maxncs) rs'

      ef = EchelonForm maxnrs maxncs

      extendRows mt mb t b rs =
        V.replicate (mt - t) (EFR.zero maxncs)
        <> rs <>
        V.replicate (b - mb) (EFR.zero maxncs)

instance AdditiveSemigroup a => AdditiveSemigroup (EchelonForm a)

instance Abelian a => Abelian (EchelonForm a)

--------------------------------------------------------------------------------
-- determinant and invertibility
--------------------------------------------------------------------------------

det :: ( Ring a, DecidableZero a ) => EchelonForm a -> a
det ef@(EchelonForm nrs ncs _)
  | nrs /= ncs = MS.zero
  | otherwise  =
      let dsMay = mapPivot (\ix jx d -> if ix == jx then Just d else Nothing) ef
      in  fromMaybe MS.zero $ foldlM (\a a' -> (a*) <$> a') one dsMay
