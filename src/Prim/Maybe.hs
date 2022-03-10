{-# language NoImplicitPrelude #-}
module Prim.Maybe where
-- import {-# source #-} Prim.B
import Type

-- | Primitive maybe type represented by a tag and (possibly invalid) value.
type Maybe# (a ∷ T_ r)  = (# B , a #)
