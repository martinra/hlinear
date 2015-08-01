{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , RankNTypes
  #-}

module HLinear.PLE.Sliced.Echelonize.Implementation
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Arrow ( (***) )
import Control.Monad.Par
import Data.Vector ( Vector )
import Data.Traversable ( for, forM )
import qualified Data.Vector as V
import Math.Structure

import HLinear.Matrix ( Matrix )
import qualified HLinear.Matrix as M
import HLinear.PLE.Decomposition.Definition
import HLinear.PLE.FoldUnfold.Echelonize ( firstHook )
import HLinear.PLE.Hook.Definition
import HLinear.PLE.Hook.PLMatrix
import qualified HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Sliced.Echelonize.Definition


instance
     ( DecidableZero a, DivisionRing a, NFData a )
  => HasPLEDecompositionSliced Matrix a
  where
  pleDecompositionSliced parameters m = runPar $ do
    ivhook <- spawnP $ firstHook m
    ivms <- for (slicedPositions parameters) $ \(ix,s) ->
              spawnP $ M.sliceCols ix s m
    pleDecompositionSlicedPar
      (slicedInnerPLEDecomposition parameters) ivhook ivms

pleDecompositionSlicedPar
  :: ( DecidableZero a, DivisionRing a, NFData a )
  => (Matrix a -> PLEDecomposition a)
  -> IVar (PLEHook a)
  -> Vector (IVar (Matrix a))
  -> Par (PLEDecomposition a)
pleDecompositionSlicedPar innerPLEDecomposition ivhook ivms_
  | V.null ivms_ = PLEDecomposition <$> get ivhook
  | otherwise = do
      let ivm = V.head ivms_
      let ivms = V.tail ivms_
    
      ivple <- spawn $ unPLEDecomposition <$> innerPLEDecomposition <$> get ivm
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
    
      pleDecompositionSlicedPar innerPLEDecomposition ivhook' ivms'
