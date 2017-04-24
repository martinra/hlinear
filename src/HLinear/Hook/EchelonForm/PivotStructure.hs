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

pivotStructure
  :: DecidableZero a
  => EchelonForm a -> PivotStructure
pivotStructure (EchelonForm nrs ncs rs) = PivotStructure $ go S.empty rs 0 0
  where
    go s rs ix jx
      | V.null rs = s
      | Just jx' <- EFR.pivotIx' (V.head rs) jx =
          go (s S.|> (ix,jx')) (V.tail rs) (succ ix) (succ jx')
      | otherwise = s

rank :: DecidableZero a => EchelonForm a -> Natural
rank e@(EchelonForm _ _ rs) = fromIntegral $ S.length $ fromPivotStructure $ pivotStructure e
