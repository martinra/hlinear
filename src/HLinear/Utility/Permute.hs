module HLinear.Utility.Permute
where

import Prelude hiding ( (+), (-), (*), recip )

import Control.DeepSeq ( NFData(..) )
import Control.Monad ( replicateM )
import Data.Permute
import Math.Structure
import Test.QuickCheck as QC
import Test.SmallCheck.Series as SC


instance NFData Permute where
  rnf p = seq p ()


instance Arbitrary Permute where
  arbitrary = do
    QC.Positive n <- arbitrary
    QC.Positive s <- arbitrary
    swaps <- replicateM s $
      do i <- arbitrary
         j <- arbitrary
         return (i `mod` n, j `mod` n)
    return $ swapsPermute n swaps

  shrink p = [ swapsPermute n (s:ss) | s <- ss ]
    where
      ss = swaps p
      n = size p

instance Monad m => Serial m Permute where
  series = do
    SC.Positive n <- series
    SC.Positive s <- series
    swaps <- replicateM s $
      do i <- series
         j <- series
         return (i `mod` n, j `mod` n)
    return $ swapsPermute n swaps


instance MultiplicativeMagma Permute where
  -- note: we let permutations act from the left
  p * p' = 
    let n = size p
        n' = size p'
        n'' = max n n'
        safeAt p n i = if i < n then p `at` i else i
    in  listPermute n''
        [ safeAt p' n' $ safeAt p n ix 
        | ix <- [0..n''-1]]

instance MultiplicativeSemigroup Permute

instance MultiplicativeMonoid Permute where
  one = permute 0

instance MultiplicativeGroup Permute where
  recip = inverse
