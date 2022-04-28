{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module BareBonesHttp.Http.Capabilities (HasCap(..), AddCap(..), emptyCap) where

class HasCap a m where
  getCap :: m -> a

class AddCap a m1 m2 | a m1 -> m2 where
  addCap :: a -> m1 -> m2

instance {-# OVERLAPPING #-} HasCap a (a, t) where
  getCap = fst

instance HasCap a t => HasCap a (b, t) where
  getCap t = getCap $ snd t

instance AddCap a t (a, t) where
  addCap a t = (a, t)

emptyCap :: ()
emptyCap = ()
