module HLinear.MultiMod.Reconstruction
where

import Control.Parallel.Strategies ( using, rseq, parBuffer )
import Data.Bits ( shiftR )
import Data.Maybe
import HLinear.MultiMod.Definition
import HFlint.Primes ( primesAfter )
import HFlint.NMod ( FlintLimb )


data ReconstructionParameters =
  ReconstructionParameters
  { bufferSize :: Int
  , reconstructionSkip :: Int
  }

reconstruct
  :: Reconstructible f d a
  => ReconstructionParameters
  -> (RReconst f d a -> Bool)
  -> MultiMod f -> f a
reconstruct parameters isReconstruction mf = fa
  where
  RReconst _ _ fa = head $ dropWhile ( not . isReconstruction ) $
                    catMaybes rationalReconstructions
  rationalReconstructions =
    map fromApprox $
    takeEveryNth (reconstructionSkip parameters) $
    scanl1 mappend approximations

  approximations =
     map (toApprox mf) (primesAfter $ (maxBound :: FlintLimb) `shiftR` 2)
     `using` parBuffer (bufferSize parameters) rseq

takeEveryNth :: Int -> [a] -> [a]
takeEveryNth n = takeEveryNth' (pred n)
  where
  takeEveryNth' n' as =
    case drop n' as of
      (a':as') -> a':takeEveryNth' n' as'
      []       -> []
