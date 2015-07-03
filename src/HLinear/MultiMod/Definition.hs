{-# LANGUAGE
    FunctionalDependencies
  , RankNTypes
  #-}

module HLinear.MultiMod.Definition
where

import HFlint.FMPQ ( FMPQ )
import qualified HFlint.FMPQ as FMPQ
import Data.List.Split ( splitEvery )
import Data.Traversable ( for )
import Control.Parallel.Strategies


-- -- a approximates b to quality q
-- class Approximates a b q | a -> b, a -> q where
--   approximate :: b -> a
--   increaseQuality :: a -> a
--   reconstruct :: (b -> q -> Bool) -> a -> b


-- newtype MultiMod f where
--   MultiMod :: ReifiesNModCtx ctx => f (NMod ctx) -> MultiMod f
-- 
-- --class MultiModReducible Matrix where
-- --class MultiModReconstructible (PLREDecomposition FMPQ)  where
-- --newtype PLREDecomposition = Permutation LeftTransformation EchelonReduction ReducedEchelonForm
-- 
-- reduce :: ReifiesNModCtx ctx => Matrix FMPQ -> ctx -> Maybe (Matrix (NMod ctx))
-- reduce ctx m@(Matrix nrs ncs rs) = Matrix nrs ncs <$>
--   V.sequence ( V.map (V.sequence . V.map (FMPQ.reduce ctx)) rs )
-- 
-- initialPrime :: Word64
-- initialPrime = nextPrime 2^60
-- 
-- newtype MultiModMatrix = MultiModMatrix
--   { nmbRows :: Natural
--   , nmbCols :: Natural
--   , reduction :: ReifiesNModCtx ctx => Maybe (Matrix (NMod ctx))
--   }
-- 
-- newtype MultiModLeftTransformation = MultiModLeftTransformation
--   { reduction :: ReifiesNModCtx ctx => Maybe (LeftTransformation (NMod ctx))
--   }
-- 
-- reduce :: Matrix FMPQ -> MultiModMatrix 
-- reduce (Matrix nrs ncs rs) = MultiModMatrix nrs ncs $
--   Matrix nrs ncs <$> V.sequence ( V.map (V.sequence . V.map FMPQ.reduce) rs )
-- 
-- instance HasPLE MultiModMatrix where
--   type PLEPermute MultiModMatrix = RPermute
--   type PLELeft MultiModMatrix    = MultiModLeftTransformation
--   type PLEEchelon MultiModMatrix = MultiModEchelonForm
--   
-- 
-- 
-- reconstruct :: ( FMPZ -> Matrix FMPQ -> Bool ) -> MultiModMatrixFMPQ -> FMPQ
-- reconstruct isFinal (MultiModMatrix nrs ncs red) =
--    head $ dropUntil (uncurry isFinal) $
--    scanl reconstruct (0, zeroMatrix nrs ncs) modularPLEs
--   where
--     reconstruct (mod, m) 
-- 
-- 
--     modularPLEs = ( map catMaybes $ splitEvery chunkSize $
--                     for primes $ \p -> (p,) <$> withNModContext p
--                                               (liftA ple . red)
--                   ) `using` evalList (parList rseq)
--     chunkSize = 20
--     primes = map fromInteger $ iterate nextPrime initialPrime
-- 
-- 
-- instance Approximates MultiModMatrixQQ (Matrix FMPQ) FMPZ where
--   approximate m@(Matrix nrs ncs _) = MultiModMatrixQQ
--     { reductions     = reduceModulo m
--     , reconstruction = return $ zeroMatrix nrs ncs
--     }
--   reconstruct isFinal mm = runIdentity $
--     evalStateT 0 $
--     evalState (FMPZ.nextPrime $ fromInteger 2^60) $
--     reconstructionS
--     
