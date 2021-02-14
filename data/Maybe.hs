{-# OPTIONS_HADDOCK ignore-exports #-}
module Maybe (module Maybe, module X) where
import B as X

-- | Primitive maybe type represented by a tag and (possibly invalid) value.
type Maybe# (a ∷ T_ r)  = (# B , a #)
