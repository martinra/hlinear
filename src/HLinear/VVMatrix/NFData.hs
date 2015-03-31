module HLinear.VVMatrix.NFData
where

import Control.DeepSeq

import HLinear.VVMatrix.Definition


instance NFData a => NFData (VVMatrix a) where
  rnf (Zero nrs ncs) = rnf nrs `seq` rnf ncs `seq` ()
  rnf (One nrs a) = rnf nrs `seq` rnf a `seq` ()
  rnf (VVMatrix nrs ncs rs) = rnf nrs `seq` rnf ncs `seq` rnf rs `seq` ()

instance NFData a => NFData (SizedVVMatrix nrs ncs a) where
  rnf (SizedVVMatrix m) = rnf m
