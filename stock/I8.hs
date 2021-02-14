module I8 (module I8, module X) where
import GHC.Int as X (Int8)
import B as X (B)

newtype I8 = I8 Int8

(≡), (≠), (<), (≤), (>), (≥) ∷ I8 → I8 → B
(≡) = coerce eqInt8
(≠) = coerce neInt8
(<) = coerce ltInt8
(≤) = coerce leInt8
(>) = coerce gtInt8
(≥) = coerce geInt8
{-# inline (≡) #-}
{-# inline (≠) #-}
{-# inline (>) #-}
{-# inline (≥) #-}
{-# inline (<) #-}
{-# inline (≤) #-}
