{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , InstanceSigs
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , StandaloneDeriving
  #-}

module HLinear.PLE.Hook.LeftTransformation.Weak
where

import Prelude hiding ( (+), (-), negate, subtract
                      , (*), (/), recip, (^), (^^)
                      , gcd
                      , quotRem, quot, rem
                      , head, tail
                      )

import Control.DeepSeq ( NFData(..) )
import Data.Proxy
import Data.Reflection
import Data.Vector ( Vector )
import qualified Data.Vector as V
import Math.Algebra.MonicExtension
import Math.Structure
import Numeric.Natural

import HLinear.PLE.Hook.LeftTransformation.Definition
 ( LeftTransformation(LeftTransformation) )
import HLinear.PLE.Hook.LeftTransformation.Column
 ( LeftTransformationColumn(LeftTransformationColumn) )
import HLinear.BRMatrix.RVector ( RVector(RVector) )
import qualified HLinear.BRMatrix.RVector as RV


 -- \ A vector of columns (a, [v]) which are offset by their index.
 --   It represents a transformation from the left
 --     a1     0     0   0
 --   v*a1    a2     0   0
 --   v*a1  v*a2    a3   0 
 --   v*a1  v*a2  v*a3  a4
 --   . . . .

data WeakLeftTransformation a =
  WeakLeftTransformation Natural (Vector (WeakLeftTransformationColumn a))

instance NFData a => NFData (WeakLeftTransformation a) where
  rnf (WeakLeftTransformation nrs cs) =
    seq (rnf nrs) $ seq (rnf cs) ()

nmbRows :: WeakLeftTransformation a -> Natural
nmbRows (WeakLeftTransformation nrs _) = nrs

columns :: WeakLeftTransformation a -> Vector (WeakLeftTransformationColumn a)
columns (WeakLeftTransformation _ cs) = cs

(!) :: WeakLeftTransformation a -> Int -> WeakLeftTransformationColumn a
(!) (WeakLeftTransformation _ cs) jx = cs V.! jx

-- columns

data WeakLeftTransformationColumn a =
  WeakLeftTransformationColumn Int a (Vector a)


offset :: WeakLeftTransformationColumn a -> Int
offset (WeakLeftTransformationColumn o _ _) = o

head :: WeakLeftTransformationColumn a -> a
head (WeakLeftTransformationColumn _ a _) = a

tail :: WeakLeftTransformationColumn a -> Vector a
tail (WeakLeftTransformationColumn _ _ as) = as


fromLeftTransformationColumn
  :: LeftTransformationColumn a -> WeakLeftTransformationColumn a
fromLeftTransformationColumn (LeftTransformationColumn o a v) =
  WeakLeftTransformationColumn o (fromNonZero a) v

deriving instance Show a => Show (WeakLeftTransformationColumn a)

instance Eq a => Eq (WeakLeftTransformationColumn a) where
  (WeakLeftTransformationColumn s a v) == (WeakLeftTransformationColumn s' a' v') =
    s == s' && a == a' && (`V.all` V.zip v v') (uncurry (==))

instance NFData a => NFData (WeakLeftTransformationColumn a) where
  rnf (WeakLeftTransformationColumn s a c) =
    seq (rnf s) $ seq (rnf a) $ seq (rnf c) ()

-- conversion

fromLeftTransformation
  :: LeftTransformation a -> WeakLeftTransformation a
fromLeftTransformation (LeftTransformation nrs cs) =
  WeakLeftTransformation nrs $ V.map fromLeftTransformationColumn cs

-- application to RVector

apply
  :: ( DivisionRing a, LinearSemiringLeftAction a b )
  => WeakLeftTransformation a -> RVector b -> RVector b
apply lt@(WeakLeftTransformation nrs cs) (RVector v) = RVector $
    V.foldr' applyCol v $ V.drop nrsDiff cs
    where
    nv = V.length v
    nrsDiff = fromIntegral nrs - nv

    -- this assumes that vn is longer than v'
    {-# INLINE applyCol #-}
    applyCol c@(WeakLeftTransformationColumn s' a' v') vn =
       V.init vn1 `V.snoc` av
       V.++
       V.zipWith (\bl br -> bl*.av + br) v' vn2
         where
         av = a' *. V.last vn1
         (vn1,vn2) = V.splitAt (V.length vn - V.length v') vn

instance Functor WeakLeftTransformation where
  fmap f (WeakLeftTransformation nrs cs) =
    WeakLeftTransformation nrs $ V.map (fmap f) cs

instance Functor WeakLeftTransformationColumn where
  fmap f (WeakLeftTransformationColumn o a as) =
    WeakLeftTransformationColumn o (f a) $ V.map f as

instance
     AdditiveMagma a
  => AdditiveMagma (WeakLeftTransformation a)
  where
  lt@(WeakLeftTransformation nrs cs) + lt'@(WeakLeftTransformation nrs' cs')
    | nrs /= nrs' = error "incompatible dimensions"
    | otherwise =
        WeakLeftTransformation nrs cs''
        where
          cs'' = V.zipWith addColumn cs cs' V.++ back
          back = case compare ncsZ ncs'Z of
                   EQ -> V.empty
                   GT -> V.drop ncs'Z cs 
                   LT -> V.drop ncsZ cs'
          addColumn (WeakLeftTransformationColumn o a as)
                    (WeakLeftTransformationColumn o' a' as') =
            WeakLeftTransformationColumn o (a+a') $ V.zipWith (+) as as'

          ncsZ = V.length cs
          ncs'Z = V.length cs'


instance
     AdditiveSemigroup a
  => AdditiveSemigroup (WeakLeftTransformation a)

instance Abelian a => Abelian (WeakLeftTransformation a)

instance
     MultiplicativeSemigroupLeftAction a b
  => MultiplicativeSemigroupLeftAction a (WeakLeftTransformationColumn b)
  where
    a *. (WeakLeftTransformationColumn o b bs) =
      WeakLeftTransformationColumn o (a*.b) $ V.map (a*.) bs
  
instance
     MultiplicativeSemigroupLeftAction a b
  => MultiplicativeSemigroupLeftAction a (WeakLeftTransformation b)
  where
    a *. (WeakLeftTransformation ncs cs) =
      WeakLeftTransformation ncs $ V.map (a*.) cs

instance
     ( LinearSemiringLeftAction a b, AdditiveSemigroup b )
  => LinearSemiringLeftAction a (WeakLeftTransformation b)


instance IntertwinesExtension WeakLeftTransformation where
  intertwineFrom
    :: forall a ctx b
    .  ( IsExtension a ctx b
       , IsExtension a ctx (WeakLeftTransformation b) )
    => Extension a ctx (WeakLeftTransformation b)
    -> WeakLeftTransformation (Extension a ctx b)

  intertwineFrom
    (Coordinates wlt) = WeakLeftTransformation nrs $
      V.generate ncsZ $ \jx -> WeakLeftTransformationColumn jx
        ( Coordinates $ V.generate d $ \dx -> head (wlt V.! dx ! jx) )
        ( V.generate (nrsZ-jx-1) $ \ix -> Coordinates $
            V.generate d $ \dx -> tail (wlt V.! dx ! jx) V.! ix
        )
      where
        d = degree (reflect (Proxy :: Proxy ctx))
        nrs = nmbRows $ V.head wlt
        nrsZ = fromIntegral nrs
        ncsZ = V.length $ columns $ V.head wlt

  intertwineFrom
    (Evaluations wlt) = WeakLeftTransformation nrs $
      V.generate ncsZ $ \jx -> WeakLeftTransformationColumn jx
        ( Evaluations $ V.generate d $ \dx ->  head (wlt V.! dx ! jx) )
        ( V.generate (nrsZ-jx-1) $ \ix -> Evaluations $
            V.generate d $ \dx -> tail (wlt V.! dx !jx) V.! ix
        )
      where
        d = degree (reflect (Proxy :: Proxy ctx))
        nrs = nmbRows $ V.head wlt
        nrsZ = fromIntegral nrs
        ncsZ = V.length $ columns $ V.head wlt

  intertwineTo
    :: forall a ctx b
    .  ( IsExtension a ctx b
       , IsExtension a ctx (WeakLeftTransformation b) )
    => WeakLeftTransformation (Extension a ctx b)
    -> Extension a ctx (WeakLeftTransformation b)
  intertwineTo wlt
    | ncsZ == 0 = Coordinates $ V.replicate d $
                    WeakLeftTransformation nrs V.empty
    | otherwise = extConst $ intertwineTo' $ fmap extVec wlt
        where
        nrs = nmbRows wlt
        nrsZ = fromIntegral nrs
        ncsZ = V.length $ columns wlt

        (d, (extConst, extVec)) =
          let e = head (wlt ! 0) :: Extension a ctx b
          in  ( degree (reflect (Proxy :: Proxy ctx))
              , case e of
                  Coordinates _ -> (Coordinates, coordinateVector)
                  Evaluations _ -> (Evaluations, evaluationVector)
              )

        intertwineTo' wlt =
          V.generate d $ \dx -> WeakLeftTransformation nrs $
          V.generate ncsZ $ \jx -> WeakLeftTransformationColumn jx
            ( head (wlt ! jx) V.! dx )
            ( V.generate (nrsZ-jx-1) $ \ix ->
                tail (wlt ! jx) V.! ix V.! dx
            )
