{-# LANGUAGE
    QuasiQuotes
  , TemplateHaskell
  #-}

module HLinear.Matrix.TH
where

import Prelude hiding ( (*) )

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Math.Structure
import qualified Data.Vector as V

import HLinear.Matrix.Definition ( Matrix(..) )


mkDecl :: Name -> ExpQ -> DecQ
mkDecl f e = funD f [clause [] (normalB e ) []]

--------------------------------------------------------------------------------
-- action of base ring
--------------------------------------------------------------------------------

baseRingAction :: CxtQ -> TypeQ -> DecsQ
baseRingAction cxt a = sequence
  [
  -- left action of the base ring
    instanceD
      ( (:) <$> ([t|MultiplicativeSemigroup|] `appT` a) <*> cxt )
      ( ([t|MultiplicativeSemigroupLeftAction|] `appT` a)
        `appT` ([t|Matrix|] `appT` a)
      )
      [ mkDecl '(*.) [| \a (Matrix nrs ncs rs) -> Matrix nrs ncs $ V.map (V.map (a*)) rs |]
      ]
  , instanceD
      ( (:) <$> ([t|MultiplicativeMonoid|] `appT` a) <*> cxt )
      ( ([t|MultiplicativeLeftAction|] `appT` a)
        `appT` ([t|Matrix|] `appT` a)
      )
      []
  , instanceD
      ( (:) <$> ([t|Semiring|] `appT` a) <*> cxt )
      ( ([t|LinearSemiringLeftAction|] `appT` a)
        `appT` ([t|Matrix|] `appT` a)
      )
      []

  -- right action of the base ring
  , instanceD
      ( (:) <$> ([t|MultiplicativeSemigroup|] `appT` a) <*> cxt )
      ( ([t|MultiplicativeSemigroupRightAction|] `appT` a)
        `appT` ([t|Matrix|] `appT` a)
      )
      [ mkDecl '(.*) [| \(Matrix nrs ncs rs) a -> Matrix nrs ncs $ V.map (V.map (*a)) rs |]
      ]
  , instanceD
      ( (:) <$> ([t|MultiplicativeMonoid|] `appT` a) <*> cxt )
      ( ([t|MultiplicativeRightAction|] `appT` a)
        `appT` ([t|Matrix|] `appT` a)
      )
      []
  , instanceD
      ( (:) <$> ([t|Semiring|] `appT` a) <*> cxt )
      ( ([t|LinearSemiringRightAction|] `appT` a)
        `appT` ([t|Matrix|] `appT` a)
      )
      []

  -- algebra structure
  , instanceD
      ( (:) <$> ([t|Rng|] `appT` a) <*> cxt )
      ( [t|Distributive|] `appT` ([t|Matrix|] `appT` a) )
      []
  , instanceD
      ( (:) <$> ([t|Rng|] `appT` a) <*> cxt )
      ( [t|Semiring|] `appT` ([t|Matrix|] `appT` a) )
      []
  , instanceD
      ( (:) <$> ([t|Commutative|] `appT` a) <*>
          ( (:) <$> ([t|Rng|] `appT` a) <*> cxt ) )
      ( ([t|SemiLeftAlgebra|] `appT` a)
        `appT` ([t|Matrix|] `appT` a)
      )
      []
  , instanceD
      ( (:) <$> ([t|Commutative|] `appT` a) <*>
          ( (:) <$> ([t|Rng|] `appT` a) <*> cxt ) )
      ( ([t|SemiRightAlgebra|] `appT` a)
        `appT` ([t|Matrix|] `appT` a)
      )
      []
  , instanceD
      ( (:) <$> ([t|Commutative|] `appT` a) <*>
          ( (:) <$> ([t|Rng|] `appT` a) <*> cxt ) )
      ( ([t|SemiAlgebra|] `appT` a)
        `appT` ([t|Matrix|] `appT` a)
      )
      []
  ]
