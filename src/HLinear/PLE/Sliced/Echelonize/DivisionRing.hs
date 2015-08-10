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

import Control.Arrow ( (***), first, second )
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
import qualified HLinear.PLE.Hook.RPermute as RP
import qualified HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Sliced.Echelonize.Definition
import HLinear.PLE.Strategy.Definition


slicingPositions
  :: PLEDecompositionSlicedParameters
  -> Natural -> Vector (Int, Int)
slicingPositions _ 0 = V.empty

slicingPositions
  ( PLEDecompositionSlicedParameters
    SlicingUnBalanced paramSizeNmb
  ) ncs
  = bigSlices V.++ smallSlices
  where
    ncsZ = fromIntegral ncs

    nmbBigSlices = fromIntegral $
      case paramSizeNmb of
        SlicingSize paramSz -> ncs `div` paramSz
        SlicingNmb paramNmb -> paramNmb

    sizeBigSlice = fromIntegral $
      case paramSizeNmb of
        SlicingSize paramSz -> paramSz
        SlicingNmb paramNmb -> ncs `div` paramNmb

    sizeSmallSlice = fromIntegral $
      case paramSizeNmb of
        SlicingSize paramSz -> ncs `mod` paramSz
        SlicingNmb paramNmb -> ncs `mod` paramNmb

    bigSlices = if sizeBigSlice == 0
                then V.empty
                else V.iterateN nmbBigSlices
                       (first (+sizeBigSlice)) (0,sizeBigSlice)
    smallSlices = if sizeSmallSlice == 0
                  then V.empty
                  else V.singleton ( sizeBigSlice * nmbBigSlices
                                   , sizeSmallSlice )

slicingPositions
  ( PLEDecompositionSlicedParameters
    SlicingBalanced (SlicingSize paramSz)
  ) ncs
  = if remainder == 0
    then normalSlices
    else normalSlices
         `V.snoc`
         ( ncsZ - remainder, remainder )
  where
    ncsZ = fromIntegral ncs
    sz = fromIntegral paramSz

    (n,r) = ncsZ `divMod` sz

    (nmbBigSlices, nmbSmallSlices, remainder) =
      if sz-1 - r > n
      then (n,0,r)
      else (n - (sz-1) + r, 1 + sz-1 - r, 0)
    nmbBothSlices = nmbBigSlices + nmbSmallSlices

    normalSlices =
      if nmbBothSlices == 0
      then V.empty
      else slicingPositions
             ( PLEDecompositionSlicedParameters
               SlicingBalanced (SlicingNmb $ fromIntegral nmbBothSlices)
             )
             ( fromIntegral $ ncsZ - remainder )


slicingPositions
  ( PLEDecompositionSlicedParameters
    SlicingBalanced (SlicingNmb nmbSlices)
  ) ncs
  = bigSlices V.++ smallSlices
  where
    nmbSlicesZ = fromIntegral nmbSlices

    nmbBigSlices = fromIntegral $ ncs `mod` nmbSlices
    nmbSmallSlices = nmbSlicesZ - nmbBigSlices

    sizeSmallSlice = fromIntegral $ ncs `div` nmbSlices
    sizeBigSlice = succ sizeSmallSlice

    bigSlices =
      if sizeBigSlice == 0
      then V.empty
      else V.iterateN nmbBigSlices
             (first (+sizeBigSlice)) (0,sizeBigSlice)
    smallSlices =
      if sizeSmallSlice == 0
      then V.empty
      else V.iterateN nmbSmallSlices
             ( first (+sizeSmallSlice) )
             ( nmbBigSlices * sizeBigSlice
             , sizeSmallSlice
             )
  

instance
     ( HasPLEStrategy Identity a, NFData a
     , DecidableZero a, DivisionRing a )
  => HasPLEDecompositionSliced Matrix a
  where
  pleDecompositionSliced parameters strat m = runPar $ do
    ivhook <- spawnP $ firstHook m
    ivms <- for (slicingPositions parameters $ M.nmbCols m) $ \(ix,s) ->
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
                               fromPLMatrix $ l *. (p *. PLMatrix m)
          (,) <$> ivem <*> ivm'

      ive' <- spawn $ V.foldl EF.blockSum e <$> traverse get ivems
      ivhook' <- spawn $ (*) <$> get ivhook <*> (PLEHook p l <$> get ive')

      pleDecompositionSlicedPar strat ivhook' ivms'
