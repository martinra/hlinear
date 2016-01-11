module HLinear.Bench.Conversion
where

import HLinear.Matrix as M
import HFlint.FMPQMat as FMPQMat


toFMPQMat :: Matrix Rational -> FMPQMat
toFMPQMat = FMPQMat.fromVectors . M.rows . fmap fromRational
