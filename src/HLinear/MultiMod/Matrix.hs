{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  , TupleSections
  #-}

module HLinear.MultiMod.Matrix
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


instance HasLimbHeight a => HasLimbHeight (Matrix a) where
  limbHeight (Matrix _ _ rs) =
    V.maximum $ V.map (V.maximum . V.map limbHeight) rs

instance Reducible Matrix FMPQ where
  reduce m = MultiMod $ \_ -> M.mapM toNModMay m

fromApproxPrimitivetoCombined
  :: Approximation Matrix () FMPZ -> Approximation Matrix () FMPZ
fromApproxPrimitivetoCombined (ApproxPrimitive () (m :: Matrix (NMod ctx))) =
  let md = NMod.modulusIntegral (Proxy :: Proxy ctx)
  in ApproxCombined () $ Mod md $ M.map FMPZ.fromNMod m
fromApproxPrimitivetoCombined a = a

instance Approximable Matrix () where
  toApprox (MultiMod m) p =
    withNModContext (fromIntegral p) $ \proxy ->
      let m' = m proxy
      in seq (rnf m') $
         case m' of 
           Nothing  -> ApproxInvalid
           Just m'' -> ApproxPrimitive () m''

  approxAppend ApproxInvalid a = a
  approxAppend a ApproxInvalid = a
  approxAppend
      (ApproxCombined _ (Mod md m))
      (ApproxPrimitive _ (m' :: Matrix (NMod ctx))) =
    let ctxProxy = Proxy :: Proxy ctx
        md' = NMod.modulusIntegral ctxProxy
    in  ApproxCombined () $ Mod (md * md') $
          withFMPZCRTNModContext md ctxProxy $ \(_ :: Proxy ctx') ->
            let mCRT = M.zipWith chineseRemainder m m' :: Matrix (FMPZCRTNMod ctx')
            in  M.map unFMPZCRTNMod mCRT
  approxAppend (ApproxCombined _ (Mod md m)) (ApproxCombined _ (Mod md' m')) =
    ApproxCombined () $ Mod (md * md') $
      withFMPZCRTContext md md' $ \(_ :: Proxy ctx) ->
      M.map unFMPZCRT ( M.zipWith chineseRemainder m m' :: Matrix (FMPZCRT ctx) )
  approxAppend m@(ApproxPrimitive () _) m'@(ApproxPrimitive () _) =
    approxAppend (fromApproxPrimitivetoCombined m) m'
  approxAppend m m' = approxAppend m' m

instance Reconstructible Matrix () FMPQ where
  fromApprox ApproxInvalid = Nothing
  fromApprox m@(ApproxPrimitive () _) = fromApprox $ fromApproxPrimitivetoCombined m
  fromApprox (ApproxCombined () (Mod modulus m)) =
    RReconst () modulus <$>
    M.mapM (rationalReconstruct Balanced modulus) m


mulMultiMod :: Matrix FMPQ -> Matrix FMPQ -> Matrix FMPQ
mulMultiMod m m' = reconstruct isReconstruction $ mmod * mmod'
  where
    isReconstruction (RReconst () modulus _) = limbHeight modulus > 1 P.+ ht P.+ ht'
    ht = limbHeight m
    ht' = limbHeight m'
    mmod = reduce m
    mmod' = reduce m'

instance MultiplicativeMagma (MultiMod Matrix) where
  (MultiMod m) * (MultiMod m') = MultiMod $ \proxy ->
      liftA2 (*) (m proxy) (m' proxy)
