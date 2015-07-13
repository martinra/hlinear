{-# LANGUAGE
    FlexibleContexts
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
import Control.Parallel.Strategies ( using, rpar, rseq, parListChunk, parList )
import Control.DeepSeq ( NFData, rnf )
import Data.Composition ( (.:) )
import Data.Proxy
import Data.Maybe
import Data.Word ( Word64 )
import qualified Data.Vector as V
import HFlint.FMPQ
import HFlint.FMPZ as FMPZ
import HFlint.NMod
import HFlint.Primes ( primesAfter )
import qualified HFlint.NMod as NMod
import Math.Structure

import HLinear.MultiMod.Definition
import HLinear.Matrix ( Matrix(..) )
import qualified HLinear.Matrix as M

import Debug.Trace ( trace )


instance HasLimbHeight a => HasLimbHeight (Matrix a) where
  limbHeight (Matrix _ _ rs) =
    V.maximum $ V.map (V.maximum . V.map limbHeight) rs

instance Reducible Matrix FMPQ where
  reduce m = MultiMod $ \_ -> M.mapM toNModMay m


type Approx f = Maybe (Modulus FMPZ, f FMPZ)
newtype ApproxRefiner f = ApproxRefiner (Approx f -> Approx f)

rnfApproxRefiner :: MultiMod Matrix -> Word64 -> ApproxRefiner Matrix
rnfApproxRefiner (MultiMod m) p = 
  withNModContext (fromIntegral p) $ \proxy ->
    let m' = m proxy
    in seq (rnf m') $ approxRefiner m'

approxRefiner
  :: forall ctx
  .  ReifiesNModContext ctx
  => Maybe (Matrix (NMod ctx))
  -> ApproxRefiner Matrix
approxRefiner mPt' = ApproxRefiner $ \approx ->
  mPt' >>= \mPt -> Just $
    let modulusPt@(Modulus modulusPtZ) = NMod.modulusIntegral (Proxy :: Proxy ctx)
    in case approx of
         Nothing ->
           ( modulusPt
             -- todo: specialize fromIntegral :: NMod -> FMPZ
           , M.map FMPZ.fromNMod mPt
           )
         Just ( modulus@(Modulus modulusZ), m ) ->
           ( Modulus $ modulusPtZ * modulusZ
           , M.zipWith (chineseRemainder modulus) m mPt
           )

instance Reconstructible Matrix FMPQ where
  reconstruct isReconstruction m = mQ
    where
    (_,mQ) = head $ dropWhile ( not . uncurry isReconstruction ) rationalReconstructions
    rationalReconstructions = catMaybes $ flip map chinesesRemainders $ \(modulus, mat) ->
      (modulus,) <$> M.mapM (rationalReconstruct Balanced modulus) mat
    chinesesRemainders = catMaybes $ scanl (\a (ApproxRefiner r) -> r a) Nothing refiners
    refiners = map (rnfApproxRefiner m) (primesAfter 0XDFFFFFFFFFFFFFFF)
-- todo: parListChunks hangs. Investigate why and implement different approach if necessary
               `using` rseq


mulMultiMod :: Matrix FMPQ -> Matrix FMPQ -> Matrix FMPQ
mulMultiMod m m' = reconstruct isReconstruction $ mmod * mmod'
  where
    ht = limbHeight m
    ht' = limbHeight m'
    mmod = reduce m
    mmod' = reduce m'
    isReconstruction modulus _ = trace ( show (limbHeight modulus) ++ show ( 1 P.+ ht P.+ ht' ) ) $ limbHeight modulus > 1 P.+ ht P.+ ht'

instance MultiplicativeMagma (MultiMod Matrix) where
  (MultiMod m) * (MultiMod m') = MultiMod $ \proxy ->
      liftA2 (*) (m proxy) (m' proxy)
