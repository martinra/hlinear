{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  , TupleSections
  #-}

module HLinear.Matrix.MultiMod
where

import Control.DeepSeq ( NFData(..), force )
import qualified Data.Vector as V
import HFlint.FMPQ
import HFlint.FMPZ as FMPZ
import HFlint.NMod
import qualified HFlint.NMod as NMod
import Math.Structure
import Numeric.Natural

import HLinear.MultiMod.Definition
import HLinear.MultiMod.Standard
import HLinear.Matrix ( Matrix(..) )
import qualified HLinear.Matrix as M


instance NFData a => NFData (Approximation Matrix () a) where
  rnf ApproxInvalid = ()
  rnf (ApproxPrimitive () (Mod md m)) = seq (rnf md) $ seq (rnf m) ()
  rnf (ApproxCombined () (Mod md m)) = seq (rnf md) $ seq (rnf m) ()

instance Approximable Matrix () where
  toApprox = defaultToApprox force (const ())
  approxAppend = defaultApproxAppend M.zipWith

instance Reconstructible Matrix () FMPQ where
  fromApprox = defaultFromApprox
