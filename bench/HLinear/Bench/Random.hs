{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HLinear.Bench.Random
where

import HLinear.Utility.Prelude
import qualified Prelude as P

import Control.Monad ( replicateM )
import Control.Monad.Loops ( iterateUntil )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State.Strict ( evalStateT )
import Data.Permute ( listPermute )
import Data.Random ( runRVar, RVar, Distribution(..), Uniform(..), StdUniform(..) )
import Data.Ratio ( Rational )
import qualified Data.Random.Extras as RE
import qualified Data.Sequence as Seq
import Data.Sequence ( ViewL((:<)) )
import qualified Data.Vector as V
import Data.Word ( Word64 )
import Numeric.Natural ( Natural )
import System.Random ( randomIO )

import HFlint.NMod ( ReifiesNModContext, NMod, Modulus(..), modulus )

import HLinear.Matrix ( Matrix(..), toMatrix )
import HLinear.Hook.EchelonForm ( EchelonForm(..) )
import HLinear.Hook.EchelonForm.Row ( EchelonFormRow(..) )
import HLinear.Hook.LeftTransformation( LeftTransformation(..) )
import HLinear.Hook.LeftTransformation.Column ( LeftTransformationColumn(..) )
import HLinear.Utility.RPermute ( RPermute(..) )
import qualified HLinear.Hook.EchelonForm as EF
import qualified HLinear.Hook.LeftTransformation as LT
import qualified HLinear.Utility.RPermute as RP


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

rMatrix :: Distribution d a => Int -> Int -> d a -> RVar (Matrix a)
rMatrix nrs ncs d =
  Matrix nrs ncs <$> V.replicateM nrs ( V.replicateM ncs $ rvar d )

-- QQ with bounded denominator
rQQbd
  :: Distribution dnum Integer
  => [Natural] -> dnum Integer -> RVar Rational
rQQbd denFactors dnum = do
  num <- rvar dnum
  nmbDenFactors <- rvar $ Uniform 0 (length denFactors)
  den <- foldl (*) 1 <$> RE.sample nmbDenFactors denFactors
  return $ fromInteger num P./ fromIntegral den

rMatrixQQbd
  :: ( Distribution dnum Integer, Distribution dden Natural )
  => Int -> Int -> dnum Integer -> Natural ->  dden Natural -> RVar (Matrix Rational)
rMatrixQQbd nrs ncs dnum nden dden = do
  denFactors <- replicateM (fromIntegral nden) $ rvar dden
  Matrix nrs ncs <$> V.replicateM nrs ( V.replicateM ncs $ rQQbd denFactors dnum )

-- Matrix with echelon forms and left transformation that have bounded denominators
rMatrixQQbdLE
  :: ( Distribution dnum Integer, Distribution dden Natural )
  => Int -> Int -> dnum Integer -> Natural ->  dden Natural -> RVar (Matrix Rational)
rMatrixQQbdLE nrs ncs dnum nden dden = do
  p <- rPermutation nrs
  lt <- rLeftTransformationQQbd nrs dnum nden dden
  ef <- rEchelonFormQQbd nrs ncs dnum nden dden 
  return $ toMatrix p * toMatrix lt * toMatrix ef

rPermutation ::
  Int -> RVar RPermute
rPermutation nrs = RPermute <$> listPermute nrs <$> RE.shuffle [0..nrs-1]

rLeftTransformationQQbd
  :: ( Distribution dnum Integer, Distribution dden Natural )
  => Int -> dnum Integer -> Natural ->  dden Natural -> RVar (LeftTransformation Rational)
rLeftTransformationQQbd nrs dnum nden dden = do
  denFactors <- replicateM (fromIntegral nden) $ rvar dden
  ncs <- rvar $ Uniform 0 nrs
  cs <- for (V.enumFromN 0 ncs) $ \jx -> do
          diag <- Unit <$> iterateUntil (/=0) (rQQbd denFactors dnum)
          LeftTransformationColumn jx diag <$> V.replicateM (nrs-jx-1) (rQQbd denFactors dnum)
  return $ LeftTransformation nrs cs 

rEchelonFormQQbd
  :: ( Distribution dnum Integer, Distribution dden Natural )
  => Int -> Int -> dnum Integer -> Natural ->  dden Natural -> RVar (EchelonForm Rational)
rEchelonFormQQbd nrs ncs dnum nden dden = do
  denFactors <- replicateM (fromIntegral nden) $ rvar dden
  nrs' <- rvar $ Uniform  0 nrs
  rs <- flip evalStateT 0 $
        V.forM (V.enumFromN 0 nrs') $ \ix -> do
          offsetCur <- get
          offsetNew <- lift $ rvar (Uniform offsetCur ncs)
          EchelonFormRow offsetNew <$> V.replicateM (ncs - fromIntegral offsetNew) (lift $ rQQbd denFactors dnum)
  return $ EchelonForm nrs ncs rs
