module HLinear.VVMatrix.Definition
where

import Data.Vector ( Vector )


data VVMatrix a = VVMatrix Int Int (Vector (Vector a))
