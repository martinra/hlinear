{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , RankNTypes
  , UndecidableInstances
  #-}

module HLinear.PLE.Sliced.Echelonize.ExtMatrixFMPQ
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
import HFlint.FMPQ
import Math.Structure
import Math.Algebra.MonicExtension as Ext
import Numeric.Natural

import HLinear.Matrix ( Matrix )
import HLinear.Matrix.Extension ( ExtMatrix )
import qualified HLinear.Matrix as M
import HLinear.PLE.Decomposition.Definition
import HLinear.PLE.Decomposition.ExtMatrixFMPQ
import HLinear.PLE.FoldUnfold.Echelonize ( firstHook )
import HLinear.PLE.Hook.Definition
import HLinear.PLE.Hook.PLMatrix as PLM
import qualified HLinear.PLE.Hook.LeftTransformation.Weak as WLT
import qualified HLinear.PLE.Hook.RPermute as RP
import qualified HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Sliced.Echelonize.Definition
import HLinear.PLE.Sliced.Echelonize.Positions
import HLinear.PLE.Strategy.Definition


instance ( HasPLEStrategy Identity (ExtMatrix FMPQ ctx)
         , ReifiesExtensionCtx FMPQ ctx )
  => HasPLEDecompositionSliced (ExtMatrix FMPQ ctx)
  where
  pleDecompositionSliced parameters strat m = runPar $ do
    let headm = case m of
                  Coordinates m' -> V.head m' 
                  Evaluations m' -> V.head m' 
    let nrs = M.nmbRows headm
    let ncs = M.nmbCols headm

    ivhook <- spawnP $ firstHook nrs ncs
    ivms <- for (slicingPositions parameters ncs) $ \(ix,s) ->
              spawnP $ Ext.map (M.sliceCols ix s) m
    pleDecompositionSlicedPar strat ivhook ivms

pleDecompositionSlicedPar
  :: ( HasPLEStrategy Identity (ExtMatrix FMPQ ctx)
     , ReifiesExtensionCtx FMPQ ctx )
  => PLEStrategy Identity (ExtMatrix FMPQ ctx)
  -> IVar (PLEHook (Extension FMPQ ctx FMPQ))
  -> Vector (IVar (ExtMatrix FMPQ ctx))
  -> Par (PLEDecomposition (ExtMatrix FMPQ ctx))
pleDecompositionSlicedPar strat ivhook ivms_
  | V.null ivms_ = PLEDecomposition <$> get ivhook
  | otherwise = do
      let ivm = V.head ivms_
      let ivms = V.tail ivms_

      ivple <- spawn $ unPLEDecomposition <$> runIdentity <$>
                 dispatchPLEStrategy strat <$> get ivm
      PLEHook p l e <- get ivple
      ivwl <- spawnP $ Ext.intertwineIn $ WLT.fromLeftTransformation l
    
      ivr <- spawnP $ fromIntegral $ EF.rank e
      (ivems, ivms') <-
        fmap V.unzip $
        forM ivms $ \ivm -> do
          wl <- get ivwl
          m <- get ivm
          r <- get ivr

          let (mConst, mv) = Ext.constructorVector $
                Ext.zipWithEval (\wl' m' -> PLM.apply wl'
                                            (fromPLMatrix $ p *. PLMatrix m'))
                                wl m
          let (ivemvM, ivm'vM) = sequenceA *** sequenceA $
                                   V.unzip $ (`V.map` mv) $
                                     (spawnP *** spawnP) . M.splitAtCols r
          ivemv <- ivemvM
          ivm'v <- ivm'vM
          let ivem = spawn ( mConst <$> V.mapM get ivemv )
          let ivm' = spawn ( mConst <$> V.mapM get ivm'v )

          (,) <$> ivem <*> ivm'

      ive' <- spawn $ V.foldl EF.blockSum e <$>
                V.map Ext.intertwineFrom <$> traverse get ivems
      ivhook' <- spawn $ (*) <$> get ivhook <*>
                                 ( PLEHook p l <$>  get ive' )

      pleDecompositionSlicedPar strat ivhook' ivms'
