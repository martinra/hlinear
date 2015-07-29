{-# LANGUAGE
    ExistentialQuantification
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  #-}

module HLinear.MultiMod.Definition
where

import Data.Proxy
import qualified Data.Vector as V
import HFlint.FMPZ
import HFlint.NMod ( NMod, ReifiesNModContext
                   , Modulus(..)
                   , FlintLimb
                   )


data MultiMod f =
  MultiMod { unMultiMod
               :: forall ctx
               .  ReifiesNModContext ctx
               => Proxy ctx -> Maybe (f (NMod ctx))
           }

class Reducible f a where
  reduce :: f a -> MultiMod f

data ModGen b f a = Mod (Modulus b) (f a)
type Mod = ModGen FMPZ
data RReconst f d a = RReconst d (Modulus FMPZ) (f a)

data Approximation f d a =
    ApproxInvalid
  | ApproxPrimitive d (ModGen FlintLimb f FlintLimb)
  | ApproxCombined  d (Mod f a)

class Approximable f d where
  toApprox :: MultiMod f -> FlintLimb -> Approximation f d FMPZ
  approxAppend :: Approximation f d FMPZ -> Approximation f d FMPZ -> Approximation f d FMPZ

instance Approximable f d => Monoid (Approximation f d FMPZ) where
  mempty = ApproxInvalid
  mappend = approxAppend

class Approximable f d => Reconstructible f d a | f a -> d where
  fromApprox :: Approximation f d FMPZ -> Maybe (RReconst f d a)
