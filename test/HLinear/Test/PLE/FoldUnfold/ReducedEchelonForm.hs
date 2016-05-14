{-# LANGUAGE
    ScopedTypeVariables
  #-}

module HLinear.Test.PLE.FoldUnfold.ReducedEchelonForm
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )


import Data.Proxy
import Data.Maybe
import qualified Data.Vector as V
import HFlint.FMPQ
import HFlint.FMPZ
import HFlint.NMod
import Math.Structure

import Test.Tasty
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC

import HLinear.Matrix

import HLinear.PLE.Decomposition ( unPLEDecomposition )
import HLinear.PLE.Hook
import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.EchelonForm.Container ()
import HLinear.PLE.Hook.EchelonTransformation as ET
import HLinear.PLE.FoldUnfold.Echelonize ( pleDecompositionFoldUnfold )
import HLinear.PLE.FoldUnfold.ReducedEchelonForm as REF
import qualified HLinear.Matrix as M

import HLinear.Test.Utils

import Debug.Trace


reducedEchelonFormProperties :: TestTree
reducedEchelonFormProperties =
  testGroup "ReducedEchelonForm"
  [ testPropertyMatrix "recombine reduceLastPivot" $
      \ef -> fromMaybe True $ do
               let _ = ef :: EchelonForm FMPQ
               let ps = pivotStructure ef
               (EchelonReduction et m ef'', (ef',ps)) <- reduceLastPivot (ef,ps)

               let efm = EF.toMatrix ef
               let ef'm = EF.toMatrix ef'
               let ef''m = EF.toMatrix ef''
               let etm = ET.toMatrix et
               let zm = M.zeroMatrix (EF.nmbRows ef'') (EF.nmbCols ef')
               let im = M.identityMatrix (EF.nmbRows ef P.- ET.nmbRows et)
               return $ M.blockSum etm im * efm == M.blockMatrixL [[ef'm,m],[zm,ef''m]]

  , testPropertyMatrix "recombine reducedEchelonForm (small prime)" $
      recombineReducedEF 3

  , testPropertyMatrix "recombine reducedEchelonForm (large prime)" $
      recombineReducedEF 1125899906842679
  ]

recombineReducedEF :: FlintLimb -> Matrix FMPZ -> Bool
recombineReducedEF p m = withNModContext p $ \(_ :: Proxy ctx) ->
  let mNMod = fmap toNMod m :: Matrix (NMod ctx)
      PLEHook _ _ ef = unPLEDecomposition $ pleDecompositionFoldUnfold mNMod
      (et,ef') = reducedEchelonForm ef
      efm = EF.toMatrix ef
      ef'm = EF.toMatrix ef'
      etm =  ET.toMatrix et
      im = M.identityMatrix (EF.nmbRows ef P.- ET.nmbRows et)
    in M.blockSum etm im * efm == ef'm
