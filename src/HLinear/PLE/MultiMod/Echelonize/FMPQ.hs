{-# LANGUAGE
    DeriveAnyClass
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module HLinear.PLE.MultiMod.Echelonize.FMPQ
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import qualified Prelude as P

import Control.DeepSeq ( NFData(..), force )
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import HFlint.FMPQ
import HFlint.FMPZ
import HFlint.NMod
import Math.Structure
import Numeric.Natural

import HLinear.PLE.Decomposition.Definition
import HLinear.PLE.FoldUnfold.Echelonize ()
import HLinear.PLE.Hook
import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.EchelonForm.Row as EFR
import qualified HLinear.PLE.Hook.EchelonForm.Container as EF
import HLinear.PLE.Hook.Container ()
import HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.PLE.Hook.LeftTransformation.Container as LT
import HLinear.PLE.Hook.RPermute as RP
import HLinear.PLE.MultiMod.Echelonize.Definition
import HLinear.PLE.Strategy.Definition
import HLinear.PLE.Strategy.NMod
import HLinear.Matrix
import HLinear.MultiMod.Definition
import HLinear.MultiMod.Reconstruction
import HLinear.MultiMod.Standard


instance
  HasPLEDecompositionFMPQMultiMod Matrix
  where
  pleDecompositionMultiMod param strat m =
    reconstruct rrParam isReconstruction $
      multiModPLE $ reduce m

      where
      PLEDecompositionMultiModParameters rrParam = param
      isReconstruction (RReconst _ (Modulus md)
                       (PLEDecomposition (PLEHook _ l e))) =
        limbHeight md > 2 P.+ limbHeight l P.+ limbHeight e P.+ mht
      mht = limbHeight m

      multiModPLE :: MultiMod Matrix -> MultiMod PLEDecomposition
      multiModPLE (MultiMod mat) = MultiMod $ \ctx -> do
        mat' <- mat ctx
        dispatchPLEStrategy (strat ctx) mat'


instance
     NFData a
  => NFData ( Approximation
                PLEDecomposition
                (RPermute,PivotStructure)
                a
            )
  where
  rnf ApproxInvalid = ()
  rnf (ApproxPrimitive d (Mod md ple)) = seq (rnf d) $ seq (rnf md) $ seq (rnf ple) ()
  rnf (ApproxCombined d (Mod md ple)) = seq (rnf d) $ seq (rnf md) $ seq (rnf ple) ()


instance Approximable PLEDecomposition (RPermute,PivotStructure) where
  toApprox = defaultToApprox force $
               \(Mod _ (PLEDecomposition (PLEHook p _ e))) ->
                 (p, EF.pivotStructure e)
  approxAppend = defaultApproxAppend $
    \f (PLEDecomposition (PLEHook p l e)) (PLEDecomposition (PLEHook p' l' e')) ->
      if p /= p' then error "approxAppend: unequal permutations"
      else PLEDecomposition $ PLEHook p ( LT.zipWith f l l' ) ( EF.zipWith f e e' )

instance Reconstructible 
    PLEDecomposition
    (RPermute,PivotStructure)
    FMPQ
  where
  fromApprox = defaultFromApprox
