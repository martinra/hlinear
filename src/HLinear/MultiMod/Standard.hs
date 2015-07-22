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
import Control.DeepSeq ( NFData, rnf )
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
defaultFromApproxPrimitiveToCombined (ApproxPrimitive d (t :: t (NMod ctx))) =
  let md = NMod.modulusIntegral (Proxy :: Proxy ctx)
  in ApproxCombined d $ Mod md $ fmap FMPZ.fromNMod t
defaultFromApproxPrimitivetoCombined a = a


instance ( HasLimbHeight a, Foldable t ) => HasLimbHeight (t a) where
  limbHeight = foldl (\ht a -> max ht $ limbHeight a) 0

instance Traversable t => Reducible t FMPQ where
  reduce t = MultiMod $ \_ -> mapM toNModMay t


defaultToApprox
  :: ( Approximation t d FMPZ -> Approximation t d FMPZ )
  -> ( forall ctx . ReifiesNModContext ctx => t (NMod ctx) -> d )
  -> MultiMod t -> FlintLimb
  -> Approximation t d FMPZ
defaultToApprox defForce defDescription (MultiMod m) p = defForce $
  withNModContext (fromIntegral p) $ \proxy ->
    let t = m proxy
    in case t of 
         Nothing  -> ApproxInvalid
         Just t' -> ApproxPrimitive (defDescription t') t'

defaultApproxAppend
  :: ( Traversable t, Ord d )
  => ( forall a b c . ( Ring a, Ring b ) => ( a -> b -> c ) -> t a -> t b -> t c )
  -> Approximation t d FMPZ -> Approximation t d FMPZ
  -> Approximation t d FMPZ
defaultApproxAppend _ ApproxInvalid a = a
defaultApproxAppend _ a ApproxInvalid = a
defaultApproxAppend defZipWith
    ap@(ApproxCombined d (Mod md t))
    ap'@(ApproxPrimitive d' (t' :: t (NMod ctx))) =
  let ctxProxy = Proxy :: Proxy ctx
      md' = NMod.modulusIntegral ctxProxy
  in  case compare d d' of
        EQ -> ApproxCombined d $ Mod (md * md') $
                withFMPZCRTNModContext md ctxProxy $ \(_ :: Proxy ctx') ->
                  let mCRT = defZipWith chineseRemainder t t' :: t (FMPZCRTNMod ctx')
                  in  fmap unFMPZCRTNMod mCRT
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
