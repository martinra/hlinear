{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  , TupleSections
  #-}

module HLinear.PLE.MultiMod.EchelonForm
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import qualified Prelude as P

import Control.Applicative ( liftA2 )
import Control.Parallel.Strategies ( using, rseq, parBuffer )
import Control.DeepSeq ( NFData(..), force )
import Data.Bits ( shiftR )
import Data.Composition ( (.:) )
import Data.Proxy
import Data.Reflection
import Data.Maybe
import Data.Word ( Word64 )
import qualified Data.Vector as V
import HFlint.FMPQ
import HFlint.FMPZ as FMPZ
import HFlint.NMod
import HFlint.Primes ( primesAfter )
import qualified HFlint.NMod as NMod
import Math.Structure
import Numeric.Natural

import HLinear.MultiMod.Definition
import HLinear.MultiMod.Standard
import HLinear.Matrix ( Matrix(..) )
import qualified HLinear.Matrix as M
import HLinear.PLE.Hook.EchelonForm as EF
import qualified HLinear.PLE.Hook.EchelonForm.Container as EF
import HLinear.PLE.Hook.EchelonForm.Row as EFR


instance NFData a => NFData (Approximation EchelonForm PivotStructure a) where
  rnf ApproxInvalid = ()
  rnf (ApproxPrimitive d ef) = seq (rnf d) $ seq (rnf ef) $ ()
  rnf (ApproxCombined d (Mod md ef)) = seq (rnf d) $ seq (rnf md) $ seq (rnf ef) $ ()


instance Approximable EchelonForm PivotStructure where
  toApprox = defaultToApprox force EF.pivotStructure
  approxAppend = defaultApproxAppend EF.zipWith

instance Reconstructible EchelonForm PivotStructure FMPQ where
  fromApprox = defaultFromApprox
