module HLinear.NormalForm.FoldUnfold.RREF.DivisionRing
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
import HLinear.Hook.PLEHook ( PLEHook(..), PLREHook(..), RREF(..) )
import HLinear.Matrix.Block ( headRows, tailRows )
import HLinear.Matrix.Definition ( Matrix(..) )
import HLinear.NormalForm.PLE ( ple, HasPLE )
import qualified HLinear.Hook.EchelonForm as EF
import qualified HLinear.Hook.EchelonForm.Row as EFR
import qualified HLinear.Hook.EchelonTransformation as ET
import qualified HLinear.Matrix.Basic as M


type HasRREF a =
  ( DivisionRing a, DecidableZero a, HasPLE a )

rref :: HasRREF a => Matrix a -> PLREHook a
rref m =
  let h@(PLEHook p l e) = ple m
      RREF r e' = reduceEchelonForm e
  in  PLREHook p l r e'


reduceEchelonForm
  :: ( DivisionRing a, DecidableZero a )
  => EchelonForm a -> RREF a
reduceEchelonForm ef =
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
  :: ( DivisionRing a, DecidableZero a )
  => (EchelonForm a, PivotStructure)
  -> Maybe (ERHook a, (EchelonForm a, PivotStructure))
reduceLastPivot ( ef@(EchelonForm nrs ncs _), PivotStructure pivots )
  | nrs == 0 && ncs == 0 = Nothing
  | pivots' :> pivotRC <- viewr pivots = Just $
      let (efLeft, Matrix nrs' ncs' efTopRight, efBottomRight) =
            EF.splitAtHook pivotRC ef

          pivotRow = efBottomRight `EF.atRow` 0
          -- since we reduce EchelonForms over a division ring,
          -- the pivot entry is one
          pivotTop = V.map V.head efTopRight 
          pivotTopNormalization =  pivotTop

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
