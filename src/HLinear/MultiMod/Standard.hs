{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  , TupleSections
  #-}

module HLinear.MultiMod.Standard
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )
import qualified Prelude as P

import Control.Applicative ( liftA2 )
import Control.DeepSeq ( NFData, rnf, force )
import Data.Composition ( (.:) )
import Data.Proxy
import Data.Reflection
import Data.Maybe
import Data.Foldable( for_ )
import Data.Word ( Word64 )
import qualified Data.Vector as V
import HFlint.FMPQ
import HFlint.FMPZ as FMPZ
import HFlint.NMod
import qualified HFlint.NMod as NMod
import Math.Structure
import Numeric.Natural

import HLinear.MultiMod.Definition
import HLinear.Matrix ( Matrix(..) )
import qualified HLinear.Matrix as M


defaultFromApproxPrimitiveToCombined
  :: Traversable t
  => Approximation t d FMPZ -> Approximation t d FMPZ
defaultFromApproxPrimitiveToCombined (ApproxPrimitive d (Mod (Modulus md) t)) =
  ApproxCombined d $ Mod (Modulus $ fromIntegral md) $ fmap FMPZ.fromFlintLimb t
defaultFromApproxPrimitivetoCombined a = a


instance ( HasLimbHeight a, Foldable t ) => HasLimbHeight (t a) where
  limbHeight = foldl (\ht a -> max ht $ limbHeight a) 0

instance Traversable t => Reducible t FMPQ where
  reduce t = MultiMod $ \_ -> mapM toNModMay t


defaultToApprox
  :: Functor t 
  => ( Approximation t d FMPZ -> Approximation t d FMPZ )
  -> ( ModGen FlintLimb t FlintLimb -> d )
  -> MultiMod t -> FlintLimb
  -> Approximation t d FMPZ
defaultToApprox defForce defDescription (MultiMod m) p = defForce $
  withNModContext (fromIntegral p) $ \proxy ->
    let t = fmap unNMod <$> m proxy
    in case t of 
         Nothing -> ApproxInvalid
         Just t' -> let tmd = Mod (Modulus p) t'
                    in  ApproxPrimitive (defDescription tmd) tmd

defaultApproxAppend
  :: forall t d 
  .  ( Traversable t, Ord d )
  => ( forall a b c
       .  ( DecidableZero a, DecidableZero b
          , DecidableOne a, DecidableOne b
          , Ring a, Ring b )
       => ( a -> b -> c ) -> t a -> t b -> t c )
  -> Approximation t d FMPZ -> Approximation t d FMPZ
  -> Approximation t d FMPZ
defaultApproxAppend _ ApproxInvalid a = a
defaultApproxAppend _ a ApproxInvalid = a
defaultApproxAppend defZipWith
    ap@(ApproxCombined d (Mod md t))
    ap'@(ApproxPrimitive d' (Mod md' t')) =
  case compare d d' of
    EQ -> ApproxCombined d $ Mod ( md *  (fromIntegral <$> md') ) $
            withFMPZCRTFlintLimbContext md md' $ \(_ :: Proxy ctx') ->
              let mCRT = defZipWith chineseRemainder t t' :: t (FMPZCRTFlintLimb ctx')
              in  fmap unFMPZCRTFlintLimb mCRT
    GT -> ap
    LT -> ap'
defaultApproxAppend defZipWith
    ap@(ApproxCombined d (Mod md (t :: t FMPZ)))
    ap'@(ApproxCombined d' (Mod md' t')) =
  case compare d d' of
    EQ -> ApproxCombined d $ Mod (md * md') $
            withFMPZCRTContext md md' $ \(_ :: Proxy ctx') ->
            fmap unFMPZCRT ( defZipWith chineseRemainder t t' :: t (FMPZCRT ctx') )
    GT -> ap
    LT -> ap'
defaultApproxAppend defZipWith ap@(ApproxPrimitive _ _) ap'@(ApproxPrimitive _ _) =
  defaultApproxAppend defZipWith (defaultFromApproxPrimitiveToCombined ap) ap'
defaultApproxAppend defZipWith ap ap' = defaultApproxAppend defZipWith ap' ap
 
defaultFromApprox
  :: Traversable t
  => Approximation t d FMPZ -> Maybe (RReconst t d FMPQ)
defaultFromApprox ApproxInvalid = Nothing
defaultFromApprox ap@(ApproxPrimitive _ _) =
  defaultFromApprox $ defaultFromApproxPrimitiveToCombined ap
defaultFromApprox (ApproxCombined d (Mod md t)) =
  RReconst d md <$> mapM (rationalReconstruct Balanced md) t
