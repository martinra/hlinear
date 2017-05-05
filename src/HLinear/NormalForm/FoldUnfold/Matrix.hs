module HLinear.NormalForm.FoldUnfold.Matrix
where

import Control.Arrow ( (&&&) )
import Data.Maybe
import Data.Vector ( Vector )
import qualified Data.Vector as V

import HLinear.Matrix.Definition ( Matrix(..) )


splitOffTopLeft :: Matrix a -> Maybe ((a, Vector a), (Vector a, Vector (Vector a)))
splitOffTopLeft (Matrix nrs ncs rs)
  | nrs == 0 || ncs == 0 = Nothing
  | otherwise =
      let top = V.head rs
          (bottomLeft,bottomRight) =
            V.unzip $ V.map (V.head &&& V.tail) $ V.tail rs
      in  Just ( (V.head top, bottomLeft)
               , (V.tail top, bottomRight)
               )
