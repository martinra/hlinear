{-# LANGUAGE
    FlexibleContexts
  , MultiParamTypeClasses
  , RankNTypes
  #-}

module HLinear.MultiMod.Definition
where

import Data.Proxy
import qualified Data.Vector as V
import HFlint.FMPZ
import HFlint.NMod ( NMod, ReifiesNModContext
                   , Modulus(..)
                   )


data MultiMod f =
  MultiMod { unMultiMod
               :: forall ctx .
                  ReifiesNModContext ctx
               => Proxy ctx -> Maybe (f (NMod ctx))
           }

class Reducible f a where
  reduce :: f a -> MultiMod f
