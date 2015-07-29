{-# LANGUAGE
    DeriveAnyClass
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , TypeFamilies
  #-}

module HLinear.PLE.MultiMod.Echelonize
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import qualified Prelude as P

import Control.DeepSeq ( NFData(..), force )
import HFlint.FMPQ
import HFlint.FMPZ
import HFlint.NMod
import Math.Structure
import Numeric.Natural

import HLinear.PLE.Class
import HLinear.PLE.FoldUnfold.Echelonize ()
import HLinear.PLE.Hook
import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.EchelonForm.Row as EFR
import qualified HLinear.PLE.Hook.EchelonForm.Container as EF
import HLinear.PLE.Hook.Container ()
import HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.PLE.Hook.LeftTransformation.Container as LT
import HLinear.PLE.Hook.RPermute as RP
import HLinear.Matrix
import HLinear.MultiMod.Definition
import HLinear.MultiMod.Reconstruction
import HLinear.MultiMod.Standard

import Debug.Trace


data PLeftTransformation a = PLeftTransformation
  { pltPermute :: RPermute
  , pltLeftTransformation :: LeftTransformation a
  }

instance NFData a => NFData (PLEDecomposition a) where
  rnf (PLEDecomposition (PLEHook p l e)) =
    seq (rnf p) $ seq (rnf l) $ seq (rnf e) ()

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
                 trace ( "\n" ++
                 "toApprox: " ++ show p ++ " "
                              ++ show (EF.pivotStructure e)
                 )
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


instance NFData a => NFData (Approximation EchelonForm PivotStructure a) where
  rnf ApproxInvalid = ()
  rnf (ApproxPrimitive d (Mod md ef)) = seq (rnf d) $ seq (rnf md) $ seq (rnf ef) ()
  rnf (ApproxCombined d (Mod md ef)) = seq (rnf d) $ seq (rnf md) $ seq (rnf ef) ()

instance Approximable EchelonForm PivotStructure where
  toApprox = defaultToApprox force $ \(Mod _ ef) -> EF.pivotStructure ef
  approxAppend = defaultApproxAppend EF.zipWith

instance Reconstructible EchelonForm PivotStructure FMPQ where
  fromApprox = defaultFromApprox


instance {-# OVERLAPPING #-}
  HasPLEDecomposition Matrix FMPQ
  where
  pleDecomposition m = reconstruct isReconstruction $ multimodPLE $ reduce m
    where
    isReconstruction (RReconst _ (Modulus md) (PLEDecomposition (PLEHook p l e))) =
      trace (
        "md: " ++ show (limbHeight md) ++ " " ++
        "l: " ++ show (limbHeight l) ++ " " ++
        "e: " ++ show (limbHeight e) ++ " " ++
        "m: " ++ show (limbHeight m)
      ) $
      limbHeight md > 2 P.+ limbHeight l P.+ limbHeight e P.+ mht
    mht = limbHeight m

    multimodPLE :: MultiMod Matrix -> MultiMod PLEDecomposition
    multimodPLE (MultiMod m) = MultiMod $ \ctx -> pleDecomposition <$> m ctx
