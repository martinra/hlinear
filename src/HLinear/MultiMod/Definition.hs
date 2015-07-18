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

import Control.Parallel.Strategies ( using, rseq, parBuffer )
import Data.Bits ( shiftR )
import Data.Maybe
import Data.Proxy
import qualified Data.Vector as V
import HFlint.FMPZ
import HFlint.NMod ( NMod, ReifiesNModContext
                   , Modulus(..)
                   , FlintLimb
                   )
import HFlint.Primes ( primesAfter )


data MultiMod f =
  MultiMod { unMultiMod
               :: forall ctx
               .  ReifiesNModContext ctx
               => Proxy ctx -> Maybe (f (NMod ctx))
           }

class Reducible f a where
  reduce :: f a -> MultiMod f

data Mod f a = Mod (Modulus FMPZ) (f a)
data RReconst f d a = RReconst d (Modulus FMPZ) (f a)

data Approximation f d a =
    ApproxInvalid
  | forall ctx . ReifiesNModContext ctx => ApproxPrimitive d (f (NMod ctx))
  | ApproxCombined d (Mod f a)

class Approximable f d where
  toApprox :: MultiMod f -> FlintLimb -> Approximation f d FMPZ
  approxAppend :: Approximation f d FMPZ -> Approximation f d FMPZ -> Approximation f d FMPZ

instance Approximable f d => Monoid (Approximation f d FMPZ) where
  mempty = ApproxInvalid
  mappend = approxAppend

class Approximable f d => Reconstructible f d a | f a -> d where
  fromApprox :: Approximation f d FMPZ -> Maybe (RReconst f d a)

reconstruct :: Reconstructible f d a => (RReconst f d a -> Bool) -> MultiMod f -> f a
reconstruct isReconstruction mf = fa
  where
  RReconst _ _ fa = head $ dropWhile ( not . isReconstruction ) $
                    catMaybes rationalReconstructions
  rationalReconstructions =  map fromApprox $ takeEveryNth rrSteps $
                             scanl1 mappend approximations
  approximations =
     map (toApprox mf) (primesAfter $ (maxBound :: FlintLimb) `shiftR` 2)
     `using` parBuffer bufferSize rseq
  rrSteps = 20
  bufferSize = 3 * rrSteps

takeEveryNth :: Int -> [a] -> [a]
takeEveryNth n = takeEveryNth' (pred n)
  where
  takeEveryNth' n' as =
    case drop n' as of
      (a':as') -> a':takeEveryNth' n' as'
      []       -> []
