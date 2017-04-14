module HLinear.PLE.FoldUnfold.ReducedEchelonForm
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Data.Sequence ( ViewR(..), viewr )
import qualified Data.Sequence as S
import Data.Vector ( Vector )
import qualified Data.Vector as V
import qualified Data.Permute as P
import Math.Structure
import Numeric.Natural ( Natural )

import HLinear.PLE.Hook.Definition ( RREF(..) )
import HLinear.PLE.Hook.EchelonForm ( EchelonForm(..), PivotStructure(..) )
import qualified HLinear.PLE.Hook.EchelonForm as EF
import qualified HLinear.PLE.Hook.EchelonForm.Row as EFR
import HLinear.PLE.Hook.EchelonTransformation ( EchelonTransformation(..) )
import qualified HLinear.PLE.Hook.EchelonTransformation as ET
import HLinear.Matrix ( headRows, tailRows )
import qualified HLinear.Matrix as M
import HLinear.Matrix.Definition ( Matrix(..) )


reducedEchelonForm
  :: ( DivisionRing a, DecidableZero a )
  => EchelonForm a -> RREF a
reducedEchelonForm ef =
  let EchelonReduction et' _ ef' =
        V.foldl (*) firstReduction $
        V.unfoldr reduceLastPivot (ef, pivots)
  in  RREF et' ef'
  where
    firstReduction =
      EchelonReduction (ET.one nrs) (M.zero nrs 0) (EF.zero 0 0)
    pivots = EF.pivotStructure ef
    nrs = EF.nmbRows ef
    ncs = EF.nmbCols ef


data EchelonReduction a =
  EchelonReduction (EchelonTransformation a) (Matrix a) (EchelonForm a)
  deriving Show

instance DivisionRing a => MultiplicativeMagma (EchelonReduction a) where
  (EchelonReduction et m ef) * (EchelonReduction et' m' ef') =
    EchelonReduction
      (et'*et)
      (M.blockSumRows m' mTop)
      (EF.blockSumHook ef' mBottom ef)
    where
      (mTop,mBottom) = M.splitAtCols (fromIntegral $ M.nmbRows m') (et'*.m)

instance DivisionRing a => MultiplicativeSemigroup (EchelonReduction a)


reduceLastPivot
  :: ( DivisionRing a, DecidableZero a )
  => (EchelonForm a, PivotStructure)
  -> Maybe (EchelonReduction a, (EchelonForm a, PivotStructure))
reduceLastPivot ( ef@(EchelonForm nrs ncs rs), PivotStructure pivots )
  | nrs == 0 && ncs == 0 = Nothing
  | pivots' :> pivot <- viewr pivots = Just $
      let (efLeft, efTopRight, efBottomRight) = EF.splitAtHook pivot ef
          et = ET.singleton $ V.map negate $ M.headCols efTopRight 
          nrs'Z = fromIntegral $ EF.nmbRows efLeft
          ncs' = EF.nmbCols efBottomRight
          efBottomRightHead = M.Matrix 1 ncs' $ V.singleton $
                                efBottomRight `EF.atRow` 0
          (efTopRight',_) = M.splitAtCols nrs'Z $
                            et *. M.blockSumCols efTopRight efBottomRightHead
      in  ( EchelonReduction et efTopRight' efBottomRight
          , (efLeft, PivotStructure pivots')
          )
  | otherwise = Just
      ( EchelonReduction
          ( EchelonTransformation nrs V.empty )
          ( Matrix 0 ncs V.empty )
          ef
      , (EchelonForm 0 0 V.empty, PivotStructure S.empty)
      )
