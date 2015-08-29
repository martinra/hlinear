{-# LANGUAGE
    FlexibleContexts
  , InstanceSigs
  , ScopedTypeVariables
  #-}

module HLinear.Matrix.Extension
where

import Data.Proxy
import Data.Reflection
import qualified Data.Vector as V
import Math.Algebra.MonicExtension

import qualified HLinear.Matrix.Basic as M
import HLinear.Matrix.Definition


type ExtMatrix a ctx = Extension a ctx (Matrix a)

instance IntertwinesExtension Matrix where
  intertwineFrom
    :: forall a ctx b
    .  ( IsExtension a ctx b
       , IsExtension a ctx (Matrix b) )
    => Extension a ctx (Matrix b)
    -> Matrix (Extension a ctx b)

  intertwineFrom (Coordinates ms) = Matrix nrs ncs $
    V.generate nrsZ $ \ix ->
    V.generate ncsZ $ \jx ->
    Coordinates $ V.generate d $
      \dx -> ms V.! dx M.! ix V.!jx 
    where
      d = degree $ reflect (Proxy :: Proxy ctx)
      nrs = nmbRows $ V.head ms
      ncs = nmbCols $ V.head ms
      nrsZ = fromIntegral nrs
      ncsZ = fromIntegral ncs

  intertwineFrom (Evaluations ms) = Matrix nrs ncs $
    V.generate nrsZ $ \ix ->
    V.generate ncsZ $ \jx ->
    Evaluations $ V.generate d $ \dx ->
      ms V.! dx M.! ix V.!jx 
    where
      d = degree $ reflect (Proxy :: Proxy ctx)
      nrs = nmbRows $ V.head ms
      ncs = nmbCols $ V.head ms
      nrsZ = fromIntegral nrs
      ncsZ = fromIntegral ncs

  intertwineIn
    :: forall a ctx b
    .  ( IsExtension a ctx b
       , IsExtension a ctx (Matrix b) )
    => Matrix (Extension a ctx b)
    -> Extension a ctx (Matrix b)
  intertwineIn m@(Matrix nrs ncs _)
    | nrs == 0 || ncs == 0 = Coordinates $ V.replicate d $
                               Matrix nrs ncs $ V.replicate nrsZ V.empty
    | otherwise = exConst $ intertwineIn' $ fmap exVec m
        where
        (d, (exConst, exVec)) =
          let e = m M.! 0 V.! 0 :: Extension a ctx b
          in  ( degree (reflect (Proxy :: Proxy ctx))
              , case e of
                  Coordinates _ -> (Coordinates, coordinateVector)
                  Evaluations _ -> (Evaluations, evaluationVector)
              )

        intertwineIn' (Matrix _ _ rs) =
          V.generate d $ \dx -> Matrix nrs ncs $
          V.generate nrsZ $ \ix ->
          V.generate ncsZ $ \jx ->
            rs V.! ix V.! jx V.! dx

        nrsZ = fromIntegral nrs
        ncsZ = fromIntegral ncs
