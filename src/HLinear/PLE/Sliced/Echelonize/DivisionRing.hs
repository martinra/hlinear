{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , RankNTypes
  , UndecidableInstances
  #-}

module HLinear.PLE.Sliced.Echelonize.DivisionRing
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Arrow ( (***), second )
import Control.Monad.Identity
import Control.Monad.Par
import Data.Vector ( Vector )
import Data.Traversable ( for, forM )
import qualified Data.Vector as V
import Math.Structure
import Numeric.Natural

import HLinear.Matrix ( Matrix )
import qualified HLinear.Matrix as M
import HLinear.PLE.Decomposition.Definition
import HLinear.PLE.FoldUnfold.Echelonize ( firstHook )
import HLinear.PLE.Hook.Definition
import HLinear.PLE.Hook.PLMatrix
import qualified HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Sliced.Echelonize.Definition
import HLinear.PLE.Strategy.Definition


slicingPositions
  :: PLEDecompositionSlicedParameters
  -> Int -> Vector (Int, Int)
slicingPositions param ncs =
  bigSlices V.++ smallSlices
  where
   nmbSlices :: Int
   nmbSlices = case slicingSizeNmb param of
     SlicingSize paramSz ->
       let paramSzZ = fromIntegral paramSz
       in  if ncs `mod` paramSzZ == 0
           then ncs `div` paramSzZ
           else 1 + ncs `div` paramSzZ
     SlicingNmb paramNmb -> fromIntegral paramNmb
        
   nmbSmallSlice = ncs `mod` nmbSlices
   nmbBigSlice = nmbSlices - nmbSmallSlice
   sizeBigSlice =
      if nmbSmallSlice == 0
      then ncs `div` nmbSlices
      else 1 + ncs `div` nmbSlices
   sizeSmallSlice = sizeBigSlice - 1

   bigSlices = V.iterateN nmbBigSlice
                 (second (+sizeBigSlice)) (0,0)
   smallSlices = V.iterateN nmbSmallSlice
                 (second (+sizeSmallSlice))
                 (V.last bigSlices)
  

instance
     ( HasPLEStrategy Identity a, NFData a
     , DecidableZero a, DivisionRing a )
  => HasPLEDecompositionSliced Matrix a
  where
  pleDecompositionSliced parameters strat m = runPar $ do
    ivhook <- spawnP $ firstHook m
    ivms <- for (slicingPositions parameters $ fromIntegral $ M.nmbCols m) $ \(ix,s) ->
              spawnP $ M.sliceCols ix s m
    pleDecompositionSlicedPar strat ivhook ivms

pleDecompositionSlicedPar
  :: ( HasPLEStrategy Identity a, NFData a
     , DecidableZero a, DivisionRing a )
  => PLEStrategy Identity a
  -> IVar (PLEHook a)
  -> Vector (IVar (Matrix a))
  -> Par (PLEDecomposition a)
pleDecompositionSlicedPar strat ivhook ivms_
  | V.null ivms_ = PLEDecomposition <$> get ivhook
  | otherwise = do
      let ivm = V.head ivms_
      let ivms = V.tail ivms_
    
      ivple <- spawn $ unPLEDecomposition <$> runIdentity <$>
                 dispatchPLEStrategy strat <$> get ivm
      PLEHook p l e <- get ivple
    
      ivr <- spawnP $ fromIntegral $ EF.rank e
      (ivems, ivms') <-
        fmap V.unzip $
        forM ivms $ \ivm -> do
          m <- get ivm
          r <- get ivr
          let (ivem, ivm') = spawnP *** spawnP $ M.splitAtCols r $
                               fromPLMatrix $ l*.(p*. PLMatrix m)
          (,) <$> ivem <*> ivm'
    
      ive' <- spawn $ V.foldl EF.blockSum e <$> traverse get ivems
      ivhook' <- spawn $ (*) <$> get ivhook <*> (PLEHook p l <$> get ive')
    
      pleDecompositionSlicedPar strat ivhook' ivms'
