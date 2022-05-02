{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BareBonesHttp.Http.Capabilities (HasCap(..), AddCap, addCap, removeCap) where
  
type AddCap a c = (a, c)

class HasCap a m where
  getCap :: m -> a

instance {-# OVERLAPPING #-} HasCap a (a, t) where
  getCap = fst

instance HasCap a t => HasCap a (b, t) where
  getCap t = getCap $ snd t

addCap :: a -> c1 -> AddCap a c1
addCap a c1 = (a, c1)

removeCap :: AddCap a c1 -> c1
removeCap = snd
