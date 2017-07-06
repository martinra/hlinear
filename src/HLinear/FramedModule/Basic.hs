module HLinear.FramedModule.Basic
where

import qualified Prelude as P
import HLinear.Utility.Prelude hiding ( null )

import Control.Monad.Loops ( whileM' )
import qualified Control.Monad.State as St
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import HLinear.FramedModule.Definition
import HLinear.Hook.EchelonForm ( EchelonForm(..) )
import HLinear.Matrix.Basic hiding ( one )
import HLinear.Matrix.Definition
import HLinear.NormalForm.RREF ( rref, HasRREF, PLREHook(..) )
import qualified HLinear.Hook.EchelonForm as EF
import qualified HLinear.Hook.EchelonForm.Row as EFR


instance HasNmbRows (FramedModule a) where
  nmbRows (FramedModuleBasis e _) = nmbRows e
  nmbRows (FramedModuleDual e _) = nmbCols e P.- nmbRows e

instance ( HasRREF a, Ring a, DecidableZero a ) => IsMatrix (FramedModule a) a where
  toMatrix = toMatrix . asFramedModuleBasis

--------------------------------------------------------------------------------
-- container
--------------------------------------------------------------------------------

-- we have to assume that f preserves Pivots, i.e. NonZero
instance Functor FramedModule where
  fmap f (FramedModuleBasis e p) = FramedModuleBasis (fmap f e) p
  fmap f (FramedModuleDual e p) = FramedModuleDual (fmap f e) p

type instance Element (FramedModule a) = Vector a

instance (HasRREF a, AdditiveMonoid a) => MonoFunctor (FramedModule a) where
  omap f (FramedModuleBasis e v) =
    let PLREHook _ _ _ e' = rref $ omap f $ toMatrix e
    in  FramedModuleBasis e' v
  omap f (FramedModuleDual e v) =
    let PLREHook _ _ _ e' = rref $ omap f $ toMatrix e
    in  FramedModuleDual e' v

--------------------------------------------------------------------------------
-- attributes
--------------------------------------------------------------------------------

null :: FramedModule a -> Bool
null = (==0) . nmbRows

--------------------------------------------------------------------------------
-- representation as basis and dual
--------------------------------------------------------------------------------

asFramedModuleBasisWithPivotStructure
  :: ( HasRREF a, Ring a, DecidableZero a )
  => FramedModule a -> (EchelonForm a, Vector Int)
asFramedModuleBasisWithPivotStructure (FramedModuleBasis e v) = (e, v)
asFramedModuleBasisWithPivotStructure (FramedModuleDual e v)  =
  dualEchelonFormWithPivotStructure e v

asFramedModuleBasis
  :: ( HasRREF a, Ring a, DecidableZero a )
  => FramedModule a -> EchelonForm a
asFramedModuleBasis = fst . asFramedModuleBasisWithPivotStructure


asFramedModuleDualWithPivotStructure
  :: ( HasRREF a, Ring a, DecidableZero a )
  => FramedModule a -> (EchelonForm a, Vector Int)
asFramedModuleDualWithPivotStructure (FramedModuleDual e v)  = (e, v)
asFramedModuleDualWithPivotStructure (FramedModuleBasis e v) =
  dualEchelonFormWithPivotStructure e v

asFramedModuleDual
  :: ( HasRREF a, Ring a, DecidableZero a )
  => FramedModule a -> EchelonForm a
asFramedModuleDual = fst . asFramedModuleDualWithPivotStructure


dualEchelonFormWithPivotStructure
  :: forall a . ( HasRREF a, Ring a, DecidableZero a )
  => EchelonForm a -> Vector Int -> (EchelonForm a, Vector Int)
dualEchelonFormWithPivotStructure e@(EchelonForm nrs ncs rs) pivots =
  (e', EF.pivotVector e')
  where
    PLREHook _ _ _ e' = rref $ Matrix (ncs P.- nrs) ncs (rs'Init <> rs'Tail)
    pivotPairs :: Vector (Int,Int)
    pivotPairs = V.zip pivots $ V.tail pivots `V.snoc` fromIntegral ncs
    create' px jx =
      V.create $ do
        r' <- MV.new (fromIntegral ncs)
        forM_ (V.enumFromN 0 (px+1)) $ \px' ->
          MV.write r' (pivots V.! px') $ EF.at e px' jx 
        MV.write r' jx (negate one)
        return r'
    rs'Init = V.generate (V.head pivots) $ create' (-1)
    rs'Tail = msum $ (`V.imap` pivotPairs) $ \px (jx,jx') ->
                fmap (create' px) $ V.enumFromN (jx+1) (jx'-jx-1)

--------------------------------------------------------------------------------
-- creation and merging
--------------------------------------------------------------------------------

kernelLeftAction
  :: ( HasRREF a, DecidableZero a )
  => Matrix a -> FramedModule a
kernelLeftAction m =
  let PLREHook _ _ _ e = rref m
  in  FramedModuleDual e (EF.pivotVector e)

kernelRightAction
  :: ( HasRREF a, DecidableZero a )
  => Matrix a -> FramedModule a
kernelRightAction = kernelLeftAction . transpose


span
  :: ( HasRREF a, Ring a, DecidableZero a )
  => FramedModule a -> FramedModule a -> FramedModule a
span m m' =
  let ep  = asFramedModuleBasisWithPivotStructure m
      ep' = asFramedModuleBasisWithPivotStructure m'
      e'' = mergeEchelonForms ep ep'
  in FramedModuleBasis e'' (EF.pivotVector e'')

intersect
  :: ( HasRREF a, Ring a, DecidableZero a )
  => FramedModule a -> FramedModule a -> FramedModule a
intersect m m' =
  let ep  = asFramedModuleDualWithPivotStructure m
      ep' = asFramedModuleDualWithPivotStructure m'
      e'' = mergeEchelonForms ep ep'
  in FramedModuleDual e'' (EF.pivotVector e'')


-- todo: In the last step we compute the echelon form via a matrix.
-- In particular for small spaces in large ones this is rather inefficient.
-- Define an AlmostEchelonForm which allows from more efficient PLE.
mergeEchelonForms
  :: ( HasRREF a, AdditiveMonoid a )
  => (EchelonForm a, Vector Int) -> (EchelonForm a, Vector Int)
  -> EchelonForm a
mergeEchelonForms (EchelonForm nrs ncs rs,pivots) (EchelonForm nrs' ncs' rs',pivots')
  | ncs == ncs' = e''
  | otherwise = error "FramedModule.mergeEchelonForms: unequal number of columns"
  where
    PLREHook _ _ _ e'' = rref $ Matrix (nrs+nrs') ncs $
                           if V.last pivots >= V.last pivots'
                           then mergeRows rs rs' pivots pivots'
                           else mergeRows rs' rs pivots' pivots
    mergeRows rs rs' pivots pivots' = fmap EFR.toVector $ msum rs''Init <> rs''Tail
      where
        (rs''Init, rx') =
          (`St.runState` 0) $ V.forM (V.zip rs pivots) $ \(r,p) ->
            fmap (`V.snoc` r) $
              whileM' ((<= p) <$> (pivots' V.!) <$> St.get) $ do
                r' <- (rs' V.!) <$> St.get
                St.modify succ
                return r'
        rs''Tail = V.drop rx' rs'
