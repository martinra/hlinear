module HLinear.Hook.PLEHook.Container
where

import HLinear.Hook.EchelonForm.Container
import HLinear.Hook.LeftTransformation.Container
import HLinear.Hook.PLEHook.Definition


instance Functor PLEHook where
  fmap f (PLEHook p l e) = PLEHook p (fmap f l) (fmap f e)

instance Foldable PLEHook where
  foldl f a (PLEHook p l e) =
    let a' = foldl f a l
    in foldl f a' e
  foldr f a (PLEHook p l e) =
    let a' = foldr f a e
    in foldr f a' l

instance Traversable PLEHook where
  traverse f (PLEHook p l e) = PLEHook p <$> traverse f l <*> traverse f e
