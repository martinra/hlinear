module HLinear.NormalForm.FoldUnfold.RREF.EuclideanDomain
where

import qualified Prelude as P
import HLinear.Utility.Prelude

import Data.Sequence ( ViewR(..), viewr )
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Permute as P

import HLinear.Hook.ERHook ( ERHook(..) )
import HLinear.Hook.EchelonForm ( EchelonForm(..), PivotStructure(..) )
import HLinear.Hook.EchelonTransformation ( EchelonTransformation(..) )
import HLinear.Hook.PLEHook ( RREF(..) )
import HLinear.Matrix ( headRows, tailRows )
import HLinear.Matrix.Definition ( Matrix(..) )
import qualified HLinear.Hook.EchelonForm as EF
import qualified HLinear.Hook.EchelonForm.Row as EFR
import qualified HLinear.Hook.EchelonTransformation as ET
import qualified HLinear.Matrix as M


rref
  :: ( EuclideanDomain a, DecidableZero a )
  => EchelonForm a -> RREF a
rref ef =
  case reduceLastPivot (ef, EF.pivotStructure ef) of
    Nothing -> RREF (ET.one nrs) (EF.zero nrs ncs)
    Just (er, efp') ->
      let ERHook et' _ ef' = V.foldl (*) er $
                               V.unfoldr reduceLastPivot efp'
      in  RREF et' ef'
  where
    nrs = nmbRows ef
    ncs = nmbCols ef


reduceLastPivot
  :: ( EuclideanDomain a, DecidableZero a )
  => (EchelonForm a, PivotStructure)
  -> Maybe (ERHook a, (EchelonForm a, PivotStructure))
reduceLastPivot ( ef@(EchelonForm nrs ncs _), PivotStructure pivots )
  | nrs == 0 && ncs == 0 = Nothing
  | pivots' :> pivotRC <- viewr pivots = Just $
      let (efLeft, Matrix nrs' ncs' efTopRight, efBottomRight) =
            EF.splitAtHook pivotRC ef

          pivotRow = efBottomRight `EF.atRow` 0
          pivot = V.head pivotRow
          pivotTop = V.map V.head efTopRight 

          (pivotTop', pivotTopNormalization) =
            V.unzip $ V.map (`quotRem` pivot) pivotTop

          et = ET.singleton $ V.map negate pivotTopNormalization

          efTopRight' =
            (\f -> V.zipWith f efTopRight pivotTopNormalization) $ \r h ->
              V.zipWith (\re pe -> re - h*pe) r pivotRow

      in  ( ERHook et (Matrix nrs' ncs' efTopRight') efBottomRight
          , (efLeft, PivotStructure pivots')
          )
  | otherwise = Just
      ( ERHook (ET.one nrs) (M.zero 0 ncs) ef 
      , (EF.zero 0 0, PivotStructure S.empty)
      )
