module HLinear.NormalForm.FoldUnfold.PLH.Normalization
where

import Prelude hiding ( negate )

import Math.Structure ( Unit(..), one, negate )
import HFlint.FMPZ ( FMPZ )


class HasPLHNormalization a where
  plhNormalization :: a -> (Unit a, a)


instance HasPLHNormalization FMPZ where
  plhNormalization a = if a < 0 then (Unit (negate one), -a) else (one, a)
