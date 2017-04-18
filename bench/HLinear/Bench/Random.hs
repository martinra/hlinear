{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HLinear.Bench.Random
where

import Prelude hiding ( (*) )

import Control.Monad ( replicateM )
import Control.Monad.Loops ( iterateUntil )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State.Strict ( evalStateT )
import Control.Monad.State ( get, put )
import Data.Proxy ( Proxy(..) )
import Data.Permute ( listPermute )
import Data.Random ( runRVar, RVar, Distribution(..), Uniform(..), StdUniform(..) )
import qualified Data.Random.Extras as RE
import qualified Data.Sequence as Seq
import Data.Sequence ( ViewL((:<)) )
import Data.Traversable ( for )
import qualified Data.Vector as V
import Data.Word ( Word64 )
import Math.Structure ( DecidableZero, isZero, NonZero(..), (*) )
import Numeric.Natural
import System.Random ( randomIO )

import HFlint.NMod ( ReifiesNModContext, NMod, Modulus(..), modulus )

import HLinear.Matrix ( Matrix(..), toMatrix )
import qualified HLinear.PLE.Hook.EchelonForm as EF
import HLinear.PLE.Hook.EchelonForm.Definition ( EchelonForm(..) )
import HLinear.PLE.Hook.EchelonForm.Row ( EchelonFormRow(..) )
import qualified HLinear.PLE.Hook.LeftTransformation as LT
import HLinear.PLE.Hook.LeftTransformation.Definition( LeftTransformation(..) )
import HLinear.PLE.Hook.LeftTransformation.Column ( LeftTransformationColumn(..) )
import qualified HLinear.Utility.RPermute as RP
import HLinear.Utility.RPermute ( RPermute(..) )


--------------------------------------------------------------------------------
-- distributions
--------------------------------------------------------------------------------

instance Distribution Uniform Natural where
  rvar (Uniform l u) = fromInteger <$> rvar (Uniform (fromIntegral $ max 0 l) (fromIntegral u))

instance (Distribution Uniform a, DecidableZero a) => Distribution Uniform (NonZero a) where
  rvar (Uniform (NonZero l) (NonZero u)) = NonZero <$> iterateUntil (not . isZero) (rvar $ Uniform l u)

instance ReifiesNModContext ctxProxy => Distribution StdUniform (NMod ctxProxy) where
  rvar StdUniform = fromInteger <$> rvar (Uniform 0 (pred $ toInteger n))
    where
    Modulus n = modulus (Proxy :: Proxy ctxProxy)

--------------------------------------------------------------------------------
-- random variables
--------------------------------------------------------------------------------

rMatrix :: Distribution d a => Natural -> Natural -> d a -> RVar (Matrix a)
rMatrix nrs ncs d =
  Matrix nrs ncs <$> V.replicateM nrsZ ( V.replicateM ncsZ $ rvar d )
  where
    nrsZ = fromIntegral nrs
    ncsZ = fromIntegral ncs

-- QQ with bounded denominator
rQQbd
  :: Distribution dnum Integer
  => [Natural] -> dnum Integer -> RVar Rational
rQQbd denFactors dnum = do
  num <- rvar dnum
  nmbDenFactors <- rvar $ Uniform 0 (length denFactors)
  den <- foldl (*) 1 <$> RE.sample nmbDenFactors denFactors
  return $ fromInteger num / fromIntegral den

rMatrixQQbd
  :: ( Distribution dnum Integer, Distribution dden Natural )
  => Natural -> Natural -> dnum Integer -> Natural ->  dden Natural -> RVar (Matrix Rational)
rMatrixQQbd nrs ncs dnum nden dden = do
  denFactors <- replicateM (fromIntegral nden) $ rvar dden
  Matrix nrs ncs <$> V.replicateM nrsZ ( V.replicateM ncsZ $ rQQbd denFactors dnum )
  where
    nrsZ = fromIntegral nrs
    ncsZ = fromIntegral ncs

-- Matrix with echelon forms and left transformation that have bounded denominators
rMatrixQQbdLE
  :: ( Distribution dnum Integer, Distribution dden Natural )
  => Natural -> Natural -> dnum Integer -> Natural ->  dden Natural -> RVar (Matrix Rational)
rMatrixQQbdLE nrs ncs dnum nden dden = do
  p <- rPermutation nrs
  lt <- rLeftTransformationQQbd nrs dnum nden dden
  ef <- rEchelonFormQQbd nrs ncs dnum nden dden 
  return $ toMatrix p * toMatrix lt * toMatrix ef

rPermutation ::
  Natural -> RVar RPermute
rPermutation nrs = RPermute <$> listPermute nrsZ <$> RE.shuffle [0..pred nrsZ]
  where
    nrsZ = fromIntegral nrs

rLeftTransformationQQbd
  :: ( Distribution dnum Integer, Distribution dden Natural )
  => Natural -> dnum Integer -> Natural ->  dden Natural -> RVar (LeftTransformation Rational)
rLeftTransformationQQbd nrs dnum nden dden = do
  denFactors <- replicateM (fromIntegral nden) $ rvar dden
  ncs <- rvar $ Uniform 0 nrs
  let ncsZ = fromIntegral ncs :: Int
  cs <- for (V.enumFromN 0 ncsZ) $ \jx -> do
          diag <- NonZero <$> iterateUntil (/=0) (rQQbd denFactors dnum)
          LeftTransformationColumn jx diag <$> V.replicateM (nrsZ-jx-1) (rQQbd denFactors dnum)
  return $ LeftTransformation nrs cs 
  where
    nrsZ = fromIntegral nrs :: Int

rEchelonFormQQbd
  :: ( Distribution dnum Integer, Distribution dden Natural )
  => Natural -> Natural -> dnum Integer -> Natural ->  dden Natural -> RVar (EchelonForm Rational)
rEchelonFormQQbd nrs ncs dnum nden dden = do
  denFactors <- replicateM (fromIntegral nden) $ rvar dden
  nrs' <- rvar $ Uniform  0 nrs
  let ncsZ = fromIntegral ncs :: Int
  let nrs'Z = fromIntegral nrs' :: Int
  rs <- flip evalStateT 0 $
        V.forM (V.enumFromN 0 nrs'Z) $ \ix -> do
          offsetCur <- get
          offsetNew <- lift $ rvar (Uniform offsetCur ncs)
          EchelonFormRow offsetNew <$> V.replicateM (ncsZ - fromIntegral offsetNew) (lift $ rQQbd denFactors dnum)
  return $ EchelonForm nrs ncs rs
