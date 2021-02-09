--------------------------------------------------------------------
-- | Description : Typed witnesses with no runtime representation
--------------------------------------------------------------------
{-# language PatternSynonyms #-}
module Prim.Proxy (Proxy,proxy#) where

-- | The type constructor @Proxy#@ is used to bear witness to some
--    type variable. It\'s used when you want to pass around proxy values
--    for doing things like modelling type applications. A @Proxy#@
--    is not only unboxed, it also has a polymorphic kind, and has no
--    runtime representation, being totally free. 
type Proxy = Proxy#
