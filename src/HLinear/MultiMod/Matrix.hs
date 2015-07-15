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
import Control.Parallel.Strategies ( using, rseq, parBuffer )
import Control.DeepSeq ( NFData, rnf )
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
import HLinear.Matrix ( Matrix(..) )
import qualified HLinear.Matrix as M

import Debug.Trace ( trace )


instance HasLimbHeight a => HasLimbHeight (Matrix a) where
  limbHeight (Matrix _ _ rs) =
    V.maximum $ V.map (V.maximum . V.map limbHeight) rs

instance Reducible Matrix FMPQ where
  reduce m = MultiMod $ \_ -> M.mapM toNModMay m


data ModMatrix a = ModMatrix (Modulus a) (Matrix a)
data ApproxMatrix a =
    TrivialApprox
  | ApproxProper (ModMatrix a)
  | ApproxRefinement Natural Natural (ModMatrix a -> ModMatrix a)

-- RR RationalReconstruction
data RRMatrix = RRMatrix (Modulus FMPZ) (Matrix FMPQ)

fromApprox :: ApproxMatrix FMPZ -> Maybe RRMatrix
fromApprox TrivialApprox = Nothing
fromApprox (ApproxProper (ModMatrix modulus m)) = RRMatrix modulus <$>
  M.mapM (rationalReconstruct Balanced modulus) m
fromApprox (ApproxRefinement nrs ncs ref) =
  let ModMatrix modulus m = ref $ ModMatrix (Modulus 1) $ M.zeroMatrix nrs ncs
  in  RRMatrix modulus <$>
        M.mapM (rationalReconstruct Balanced modulus) m


instance Monoid (ApproxMatrix FMPZ) where
  mempty = TrivialApprox

  mappend TrivialApprox a = a
  mappend a TrivialApprox = a
  mappend (ApproxProper (ModMatrix md m))
          (ApproxProper (ModMatrix md' m')) =
    ApproxProper $ ModMatrix (md*md') $
      withFMPZCRTContext md md' $ \(_ :: Proxy ctx) ->
      M.map unFMPZCRT ( M.zipWith chineseRemainder m m' :: Matrix (FMPZCRT ctx) )
  mappend (ApproxProper m) (ApproxRefinement _ _ ref) = ApproxProper $ ref m
  mappend (ApproxRefinement _ _ ref) (ApproxProper m) = ApproxProper $ ref m
  mappend (ApproxRefinement nrs ncs ref) (ApproxRefinement nrs' ncs' ref') =
    ApproxRefinement (max nrs nrs') (max ncs ncs') $ ref' . ref
  


toApproxRNF :: MultiMod Matrix -> FlintLimb -> ApproxMatrix FMPZ
toApproxRNF (MultiMod m) p = 
  withNModContext (fromIntegral p) $ \proxy ->
    let m' = m proxy
    in seq (rnf m') $ toApprox' m'

toApprox'
  :: forall ctx
  .  ReifiesNModContext ctx
  => Maybe (Matrix (NMod ctx))
  -> ApproxMatrix FMPZ
toApprox' Nothing = TrivialApprox
toApprox' (Just m') = ApproxRefinement (nmbRows m') (nmbCols m') $
                        \(ModMatrix modulus@(Modulus md) m) ->
  let ctxProxy = Proxy :: Proxy ctx
      Modulus md' = NMod.modulusIntegral ctxProxy
  in  ModMatrix
        ( Modulus $ md * md' )
        ( withFMPZCRTNModContext modulus ctxProxy $ \(_ :: Proxy ctx') ->
            let mCRT = M.zipWith chineseRemainder m m' :: Matrix (FMPZCRTNMod ctx')
            in  M.map unFMPZCRTNMod mCRT
        )

reconstruct :: (RRMatrix -> Bool) -> MultiMod Matrix -> Matrix FMPQ
reconstruct isReconstruction m = mQ
  where
  RRMatrix _ mQ = head $ dropWhile ( not . isReconstruction ) $ catMaybes rationalReconstructions
  rationalReconstructions =  map fromApprox $ takeEveryNth rrSteps $ scanl1 mappend approximations
  approximations =
     map (toApproxRNF m) (primesAfter $ (maxBound :: FlintLimb) `shiftR` 2)
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

mulMultiMod :: Matrix FMPQ -> Matrix FMPQ -> Matrix FMPQ
mulMultiMod m m' = reconstruct isReconstruction $ mmod * mmod'
  where
    ht = limbHeight m
    ht' = limbHeight m'
    mmod = reduce m
    mmod' = reduce m'
    isReconstruction (RRMatrix modulus _) = limbHeight modulus > 1 P.+ ht P.+ ht'

instance MultiplicativeMagma (MultiMod Matrix) where
  (MultiMod m) * (MultiMod m') = MultiMod $ \proxy ->
      liftA2 (*) (m proxy) (m' proxy)
