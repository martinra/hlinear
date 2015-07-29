module HLinear.MultiMod.Reconstruction
where

import Control.Parallel.Strategies ( using, rseq, parBuffer )
import Data.Bits ( shiftR )
import Data.Maybe
import HLinear.MultiMod.Definition
import HFlint.Primes ( primesAfter )
import HFlint.NMod ( FlintLimb )


reconstruct :: Reconstructible f d a => (RReconst f d a -> Bool) -> MultiMod f -> f a
reconstruct isReconstruction mf = fa
  where
  RReconst _ _ fa = head $ dropWhile ( not . isReconstruction ) $
                    catMaybes rationalReconstructions
  rationalReconstructions =  map fromApprox $ takeEveryNth rrSteps $
                             scanl1 mappend approximations
  approximations =
     map (toApprox mf) (primesAfter $ (maxBound :: FlintLimb) `shiftR` 2)
  --   debug trace
  --   `using` parBuffer bufferSize rseq
  rrSteps = 20
  bufferSize = 3 * rrSteps

takeEveryNth :: Int -> [a] -> [a]
takeEveryNth n = takeEveryNth' (pred n)
  where
  takeEveryNth' n' as =
    case drop n' as of
      (a':as') -> a':takeEveryNth' n' as'
      []       -> []
