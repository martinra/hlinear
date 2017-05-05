{-# LANGUAGE
    GeneralizedNewtypeDeriving
  #-}

module HLinear.Hook.EchelonForm.PivotStructure
where

import Control.DeepSeq ( NFData )
import Data.Maybe
import Data.Sequence ( Seq )
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Math.Structure ( DecidableZero, isZero )
import Numeric.Natural

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


pivotStructure
  :: DecidableZero a
  => EchelonForm a -> PivotStructure
pivotStructure = PivotStructure . mapPivot (\ix jx _ -> (ix,jx))

pivotEntries
  :: DecidableZero a
  => EchelonForm a -> Seq a
pivotEntries = mapPivot (\_ _ a -> a)


rank :: DecidableZero a => EchelonForm a -> Natural
rank e@(EchelonForm _ _ rs) = fromIntegral $ S.length $ fromPivotStructure $ pivotStructure e
