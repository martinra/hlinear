{-# LANGUAGE
    FlexibleInstances
  , ScopedTypeVariables
  #-}

module Main
where

import qualified Prelude as P
import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      )

import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.Maybe
import Data.Proxy
import Data.Ratio ( (%) )
import Data.Vector ( Vector )
import qualified Data.Vector as V
import HFlint.FMPQ
import HFlint.FMPZ
import Math.Structure
import qualified Test.QuickCheck as QC
import qualified Test.SmallCheck as SC
import qualified Test.SmallCheck.Series as SCS


import HLinear.PLE
import HLinear.PLE.FoldUnfold.Echelonize as Echelonize
import HLinear.PLE.FoldUnfold.ReducedEchelonForm as REF
import HLinear.PLE.MultiMod.Echelonize
import HLinear.PLE.Hook
import HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.EchelonForm.Row as EFR
import HLinear.PLE.Hook.EchelonTransformation as ET
import HLinear.PLE.Hook.EchelonTransformation.Column as ETC
import HLinear.PLE.Hook.RPermute as RP
import qualified HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.Matrix as M

import HLinear.Matrix.MultiMod
import HLinear.MultiMod.Definition as MM
import HLinear.MultiMod.Reconstruction as MM

import HFlint.NMod
import HFlint.Primes

import HLinear.Bench.Random

import Debug.Trace


main :: IO ()
main = do
  return ()





--   let matSize = 20
-- 
--   -- mat <- fmap fromRational <$> uniformRandomMatrixIO 10 matSize matSize :: IO (Matrix FMPQ)
--   -- print $ toMatrices $ pleDecomposition mat
-- 
--   m <- uniformRandomMatrixIO 10 matSize matSize :: IO (Matrix Rational)
-- 
-- 
--   -- DEBUG PLE DECOMPOSITION 
--  
--   let primes = (primesAfter $ (maxBound :: FlintLimb) `shiftR` 2)
-- 
-- 
--   m <- uniformRandomMatrixIO 10 matSize matSize :: IO (Matrix Rational)
--   let m' = fmap fromRational m :: Matrix FMPQ
--   -- let Right m = M.fromLists [[1%2,3%4],[5%4,7%4]]
--   -- let m' = fmap fromRational (m :: Matrix Rational) :: Matrix FMPQ
-- 
--   -- this runs infinitely long
--   -- print $ pleDecomposition m'
-- 
-- 
--   putStrLn "MATRIX"
--   print m
--   replicateM_ 2 $ putStrLn ""
-- 
--   -- let matApproximations = 
--   --       map (toApprox $ MM.reduce m') primes :: [Approximation Matrix () FMPZ]
--   -- let matReconstructions = map fromApprox $ scanl1 mappend matApproximations
--   --      :: [Maybe (RReconst Matrix () FMPQ)]
-- 
--   -- forM_ (take 2 matApproximations) print
--   -- let RReconst _ _ matReconst = head $ dropWhile ( not . (\(RReconst () (Modulus md) _) -> md > 2^100) ) $ catMaybes matReconstructions
--   -- print matReconst
-- 
--   -- from this we conclude that the general
--   -- reduction/reconstruction framework seems to work
-- 
-- 
--   
--   -- DEBUG PLE
--   let ple = pleDecomposition m
--   -- let pleFMPQ = fmap fromRational ple :: PLEDecomposition FMPQ
-- 
--   replicateM_ 2 $ putStrLn ""
--   putStrLn "PLE"
--   -- print ple
--   replicateM_ 2 $ putStrLn ""
-- 
--   let PLEDecomposition (PLEHook _ l e) = ple
--   print $ limbHeight ( fmap fromRational l :: LT.LeftTransformation FMPQ )
--   print $ limbHeight ( fmap fromRational e :: EchelonForm FMPQ )
-- 
-- 
-- 
--   let pleApproximationsO = map (toApprox $ MM.reduce $ (fmap fromRational ple :: PLEDecomposition FMPQ)) primes :: [Approximation PLEDecomposition (RPermute,PivotStructure) FMPZ]
-- 
--   let pleReconstructionsO = map fromApprox $ scanl1 mappend pleApproximationsO
--        :: [Maybe (RReconst PLEDecomposition (RPermute,PivotStructure) FMPQ)]
-- 
--   -- forM_ (take 2 pleApproximationsO) print
--   let RReconst _ _ pleReconstO = head $ dropWhile ( not . (\(RReconst _ (Modulus md) _) -> md > 2^100) ) $ catMaybes pleReconstructionsO
--   -- print pleReconstO
-- 
--   -- forM_ (take 2 approximations) print
--   -- replicateM_ 5 $ putStrLn ""
--   -- forM_ (take 2 pleReduction) print
--   -- from this we suspect that the ple of reduced forms is correct (assuming that reduction is correct)
-- 
--   -- next steps: compare approximations and then compare how they are being concatenated
--   let pleMultiMod =
--         let multimodPLE :: MultiMod Matrix -> MultiMod PLEDecomposition
--             multimodPLE (MultiMod m) = MultiMod $ \ctx -> pleDecomposition <$> m ctx
--         in  multimodPLE $ MM.reduce m'
--   let pleApproximations = map (toApprox pleMultiMod) primes
--         :: [Approximation PLEDecomposition (RPermute,PivotStructure) FMPZ]
-- 
-- 
--   -- forM_ (take 2 pleApproximationsO) print
--   -- replicateM_ 2 $ putStrLn ""
--   -- forM_ (take 2 pleApproximations) print
-- 
--   -- let unequalApprox = filter (uncurry (/=)) $
--   --                       P.zip pleApproximationsO pleApproximations
--   -- forM_ (head unequalApprox) print
--   -- this runs infinitely, as it seems. That is the approximations are computed correctly.
-- 
-- 
--   -- next step: Prior to rational reconstruction, there is a scanl1 mappend. We check this one
--   let scanlPleApproximations = scanl1 mappend pleApproximations
--   let scanlPleApproximationsO = scanl1 mappend pleApproximationsO
-- 
--   -- let unequalApprox = filter (uncurry (/=)) $
--   --                       P.zip scanlPleApproximationsO scanlPleApproximations
--   -- forM_ (head unequalApprox) print
--   -- this runs infinitely, as it seems. That is, the approximations are combined correctly.
-- 
-- 
--   -- next step: we don't skip approximations here, but instead reconstruct immediately
--   let pleReconstructions = map fromApprox scanlPleApproximations
--         :: [ Maybe (RReconst PLEDecomposition (RPermute,PivotStructure) FMPQ) ]
--   let pleReconstructionsO = map fromApprox scanlPleApproximationsO
--         :: [ Maybe (RReconst PLEDecomposition (RPermute,PivotStructure) FMPQ) ]
--   -- let unequalReconst = filter (uncurry (/=)) $
--   --                       P.zip pleReconstructionsO pleReconstructions
--   -- forM_ (head unequalReconst) print
--   -- this runs infinitely, as it seems. That is, reconstructions are computed correctly.
-- 
-- 
--   -- next step: we drop Nothing and invalide reconstructions
--   let isReconstruction (RReconst _ (Modulus md) (PLEDecomposition (PLEHook p l e))) = 
--         trace (
--           "mdHT " ++ show (limbHeight md) ++ " " ++
--           "lHT " ++ show (limbHeight l) ++ " " ++
--           "eHT " ++ show (limbHeight e) ++ " " ++
--           "mHT " ++ show (limbHeight m')
--         )
--         $
--         limbHeight md > 2 P.+ limbHeight l P.+ limbHeight e P.+ limbHeight m'
--    
--   let validReconstructions = dropWhile ( not . isReconstruction ) $
--                                catMaybes pleReconstructions
--   let validReconstructionsO = dropWhile ( not . isReconstruction ) $
--                                 catMaybes pleReconstructionsO
--   -- let unequalReconst = filter (uncurry (/=)) $
--   --                        P.zip validReconstructionsO validReconstructions
--   -- forM_ (head unequalReconst) print
--   -- this runs infinitely, as it seems. That is, reconstructions are filtered correctly.
-- 
--   print ple
--   replicateM_ 2 $ putStrLn ""
--   print $ head validReconstructions
--   replicateM_ 2 $ putStrLn ""
--   print $ head validReconstructionsO
--   replicateM_ 2 $ putStrLn ""
--   -- we get the same result for all three tries. This means that the problem is perhaps to be searched for in the reduction.
--   print $ ( MM.reconstruct isReconstruction pleMultiMod :: PLEDecomposition FMPQ )
--   replicateM_ 2 $ putStrLn ""
--   -- this runs infinitely: the problem seems to be in reconstruction
--   -- when removing the takeEveryNth then the approximation finishes
-- 
--   -- next step: is the 2000th reconstruction a valid one?
--   -- forM_ (zip [0..] $ take 10 pleReconstructions) $
--   --   \(ix, (Just onePleReconstruction)) ->
--   --     print ix >> print (isReconstruction onePleReconstruction)
--   -- let Just onePleReconstruction = pleReconstructions !! 20
-- 
-- 
--   -- print $ pleReconstructions !! 6
--   -- replicateM_ 2 $ putStrLn ""
--   -- print $ pleReconstructions !! 7
--   
-- 
-- 
--   print "END"
-- 
-- 
-- instance Eq (RReconst PLEDecomposition (RPermute,PivotStructure) FMPQ) where
--   (==) (RReconst (rp,ps) (Modulus md) (PLEDecomposition (PLEHook p l e)))
--        (RReconst (rp',ps') (Modulus md') (PLEDecomposition (PLEHook p' l' e'))) =
--      trace ( "compReconst " ++
--        "LT: " ++ show l ++ " " ++ show l' ++ "  " ++
--        "EF: " ++ show e ++ " " ++ show e' ++ "  " ++
--        "\n"
--      )
--      $
--      rp == rp' &&
--      ps == ps' &&
--      md == md' &&
--      p == p' &&
--      l == l' &&
--      e == e'
-- 
-- instance Show (RReconst PLEDecomposition (RPermute, PivotStructure) FMPQ) where
--   show (RReconst d md ple) = "RRConst " ++ show d ++ " " ++ show md ++ " " ++ show ple
-- 
-- 
-- instance Eq (Approximation PLEDecomposition (RPermute, PivotStructure) FMPZ) where
--   ApproxInvalid == ApproxInvalid = True
--   (==) (ApproxPrimitive (rp,ps) (Mod md (PLEDecomposition (PLEHook p l e))))
--        (ApproxPrimitive (rp',ps') (Mod md' (PLEDecomposition (PLEHook p' l' e')))) =
--      trace ( "compPrimitive " ++
--        "MD: " ++ show md ++ " " ++ show md' ++ " " ++
--        "LT: " ++ show l ++ " " ++ show l' ++ "  " ++
--        "EF: " ++ show e ++ " " ++ show e' ++ "  " ++
--        "\n"
--      )
--      $
--      md == md' &&
--      rp == rp' &&
--      ps == ps' &&
--      p == p' &&
--      l == l' &&
--      e == e'
--   (==) (ApproxCombined (rp,ps) (Mod (Modulus md) (PLEDecomposition (PLEHook p l e))))
--        (ApproxCombined (rp',ps') (Mod (Modulus md') (PLEDecomposition (PLEHook p' l' e')))) =
--      trace ( "compCombined " ++
--        "LT: " ++ show l ++ " " ++ show l' ++ "  " ++
--        "EF: " ++ show e ++ " " ++ show e' ++ "  " ++
--        "\n"
--      )
--      $
--      rp == rp' &&
--      ps == ps' &&
--      md == md' &&
--      p == p' &&
--      l == l' &&
--      e == e'
-- 
-- instance Show (Approximation Matrix () FMPZ) where
--   show ApproxInvalid = "ApproxInvalid"
--   show (ApproxPrimitive () (Mod md m)) = "ApproxPrimitive " ++ show md ++ " " ++ show m
--   show (ApproxCombined () (Mod md m)) = "ApproxCombined " ++ show md ++ " " ++ show m
-- 
-- instance Show (Approximation PLEDecomposition (RPermute, PivotStructure) FMPZ) where
--   show ApproxInvalid = "ApproxInvalid"
--   show (ApproxPrimitive d (Mod md ple)) = "ApproxPrimitive " ++ show d ++ " " ++ show md ++ " " ++ show ple
--   show (ApproxCombined d (Mod md ple)) = "ApproxCombined " ++ show d ++ " " ++ show md ++ " " ++ show ple
