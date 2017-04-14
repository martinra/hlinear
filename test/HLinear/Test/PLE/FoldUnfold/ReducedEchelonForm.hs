{-# LANGUAGE
    FlexibleContexts
  , Rank2Types
  , ScopedTypeVariables
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

import HLinear.PLE.HasPLE
import HLinear.PLE.FoldUnfold.ReducedEchelonForm
import HLinear.PLE.Hook
import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.EchelonForm.Container ()
import HLinear.PLE.Hook.EchelonTransformation as ET
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

               let efm = toMatrix ef
               let ef'm = toMatrix ef'
               let ef''m = toMatrix ef''
               let etm = toMatrix et
               let zm = M.zero (EF.nmbRows ef'') (EF.nmbCols ef')
               let im = M.one (EF.nmbRows ef P.- ET.nmbRows et)
               return $ M.blockSum etm im * efm == M.blockMatrixL [[ef'm,m],[zm,ef''m]]

  , testPropertyMatrix "recombine reducedEchelonForm (small prime)" $
      recombineReducedEF 3

  , testPropertyMatrix "recombine reducedEchelonForm (large prime)" $
      recombineReducedEF 1125899906842679
  ]

recombineReducedEF :: FlintLimb -> Matrix FMPZ -> Bool
recombineReducedEF p m =
  withNModContext p $ \(_ :: ReifiesNModContext ctx => Proxy ctx) ->
    let mNMod = fmap toNMod m :: Matrix (NMod ctx)
        PLEHook _ _ e = ple mNMod
        RREF r e' = rref e
        im = M.one (EF.nmbRows e P.- ET.nmbRows r) :: Matrix (NMod ctx)
    in  (M.blockSum (toMatrix r) im) * toMatrix e == toMatrix e'
