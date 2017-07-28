module HLinear.Hook.PLEHook.Definition
where

import HLinear.Utility.Prelude

import HLinear.Matrix.Definition ( Matrix, IsMatrix(..), IsMatrixFactorization(..) )
import HLinear.Hook.EchelonForm as EF
import HLinear.Hook.EchelonTransformation as ET
import HLinear.Hook.LeftTransformation as LT
import HLinear.Utility.RPermute as RP


-- representation of a matrix m as a triple p l e satisfying
-- e = l p m  i.e.  m = (recip p) (recip l) e
data PLEHook a =
  PLEHook
    RPermute
    (LeftTransformation a)
    (EchelonForm a)
  deriving Show

-- representation of a matrix m as a triple p l u e satisfying
-- e = u l p m  i.e.  m = (recip p) (recip l) (recip u) e
data PLUEHook a =
  PLUEHook
    RPermute
    (LeftTransformation a)
    (EchelonTransformation a)
    (EchelonForm a)
  deriving Show

-- representation of an echelon form e as a pair u e' satisfying
-- e' = u e  i.e.  e' = (recip u) e
data UEHook a =
  UEHook
    (EchelonTransformation a)
    (EchelonForm a)
  deriving Show

--------------------------------------------------------------------------------
-- NFData
--------------------------------------------------------------------------------

instance NFData a => NFData (PLEHook a) where
  rnf (PLEHook p l e) =
    seq (rnf p) $ seq (rnf l) $ seq (rnf e) ()

instance NFData a => NFData (PLUEHook a) where
  rnf (PLUEHook p l r e) =
    seq (rnf p) $ seq (rnf l) $ seq (rnf r) $ seq (rnf e) ()

instance NFData a => NFData (UEHook a) where
  rnf (UEHook t e) =
    seq (rnf t) $ seq (rnf e) ()

--------------------------------------------------------------------------------
-- IsMatrix
--------------------------------------------------------------------------------

instance AdditiveMonoid a => IsMatrix (PLEHook a) a where
  toMatrix (PLEHook _ _ e) = toMatrix e

instance AdditiveMonoid a => IsMatrix (PLUEHook a) a where
  toMatrix (PLUEHook _ _ _ e) = toMatrix e

instance AdditiveMonoid a => IsMatrix (UEHook a) a where
  toMatrix (UEHook _ e) = toMatrix e

--------------------------------------------------------------------------------
-- IsMatrixFactorization
--------------------------------------------------------------------------------

instance
     ( Ring a, DecidableUnit a )
  => IsMatrixFactorization (PLEHook a) a
  where
  toMatrices (PLEHook p l e) =
    [ toMatrix (recip p)
    , toMatrix (recip l)
    , toMatrix e
    ]

instance
     ( Ring a, DecidableUnit a )
  => IsMatrixFactorization (PLUEHook a) a
  where
  toMatrices (PLUEHook p l u e) =
    [ toMatrix (recip p)
    , toMatrix (recip l)
    , toMatrix (recip u)
    , toMatrix e
    ]

instance Ring a => IsMatrixFactorization (UEHook a) a where
  toMatrices (UEHook u e) =
    [ toMatrix (recip u)
    , toMatrix e
    ]
