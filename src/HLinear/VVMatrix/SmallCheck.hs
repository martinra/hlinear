{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  #-}

module HLinear.VVMatrix.SmallCheck
where

import Control.Monad.Identity ( Identity(..) )
import qualified Data.Vector as V
import Test.SmallCheck.Series ( Serial
                              , Series(..)
                              , generate
                              , list
                              , series
                              )

import HLinear.VVMatrix.Basic ( fromLists )
import HLinear.VVMatrix.Definition ( VVMatrix(..) )

instance (Monad m, Serial Identity a)
      => Serial m (VVMatrix a)
  where
  series = generate $ \depth ->
           [ VVMatrix nrs ncs rs
           | nrs <- [0..depth]
           , ncs <- [0..depth]
           , rs <- V.replicateM nrs $ V.replicateM ncs $
                   list depth (series :: Series Identity a)
           ]
