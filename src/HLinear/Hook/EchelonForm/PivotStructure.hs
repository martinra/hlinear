module HLinear.Hook.EchelonForm.PivotStructure
where

import HLinear.Utility.Prelude

import Data.Sequence ( Seq )
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import HLinear.Hook.EchelonForm.Definition
import qualified HLinear.Hook.EchelonForm.Row as EFR


newtype PivotStructure = PivotStructure 
  { fromPivotStructure :: Seq (Int,Int) }
  deriving (Show, Eq, Ord, NFData)

mapPivot
  :: DecidableZero a
  => (Int -> Int -> a -> b) -> EchelonForm a -> Seq b
mapPivot f (EchelonForm nrs ncs rs) = go S.empty rs 0 0
  where
    go s rs ix jx
      | V.null rs = s
      | Just (jx',a) <- EFR.pivotIxEntry' jx (V.head rs) =
          go (s S.|> f ix jx' a) (V.tail rs) (succ ix) (succ jx')
      | otherwise = s

-- todo: as soon as traversables for Seq are in LTS, use the
-- mutable code
seqToVector :: Seq a -> Vector a
seqToVector = V.fromList . toList
--  V.create $ do
--    v <- MV.new (S.length seq)
--    void $ S.traverseWithIndex (\ix a -> MV.write v (np-ix-1) a) seq
--    return v

pivotStructure
  :: DecidableZero a
  => EchelonForm a -> PivotStructure
pivotStructure = PivotStructure . mapPivot (\ix jx _ -> (ix,jx))

pivotVector
  :: DecidableZero a
  => EchelonForm a -> Vector Int
pivotVector = fmap snd . seqToVector . fromPivotStructure . pivotStructure


pivotEntries
  :: DecidableZero a
  => EchelonForm a -> Seq a
pivotEntries = mapPivot (\_ _ a -> a)

pivotEntryVector
  :: DecidableZero a
  => EchelonForm a -> Vector a
pivotEntryVector = seqToVector . pivotEntries
  

rank :: DecidableZero a => EchelonForm a -> Int
rank = (`execState` 0) . sequenceA_ . mapPivot (\_ _ _ -> modify succ)


hasNonzeroDiagonal :: DecidableZero a => EchelonForm a -> Bool
hasNonzeroDiagonal = and . mapPivot (\ix jx _ -> ix == jx)

hasUnitDiagonal :: (DecidableZero a, DecidableUnit a) => EchelonForm a -> Bool
hasUnitDiagonal = and . mapPivot (\ix jx a -> ix == jx && isUnit a)
