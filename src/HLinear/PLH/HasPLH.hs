{-# LANGUAGE
    FlexibleContexts
  , TypeFamilies
  , UndecidableInstances
  #-}

module HLinear.PLH.HasPLH
where

import Math.Structure ( EuclideanDomain, DecidableZero, DecidableUnit, MultiplicativeGroup, Unit )

import HLinear.Matrix ( Matrix )
import HLinear.PLE.Hook ( PLEHook(..) )
import HLinear.PLH.FoldUnfold.EuclideanDomain as FUED


class HasPLH a where
  type PLH a :: *

  plh :: a -> PLH a

instance {-# OVERLAPPABLE #-}
  ( EuclideanDomain a, DecidableZero a, DecidableUnit a, MultiplicativeGroup (Unit a) )
  => HasPLH (Matrix a)
  where
    type PLH (Matrix a) = PLEHook a
    plh = FUED.plhFoldUnfold