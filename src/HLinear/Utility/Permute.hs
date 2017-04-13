{-# LANGUAGE
    FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  #-}

module HLinear.Utility.Permute
where

import Prelude hiding ( (+), (-), (*), recip )

import Control.DeepSeq ( NFData(..) )
import Data.Permute
import Math.Structure


instance NFData Permute where
  rnf p = seq p ()


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
