module I32 (module I32, module X) where
import GHC.Int as X (Int32)
import B as X (B)

newtype I32 = I32 Int32

(≡), (≠), (<), (≤), (>), (≥) ∷ I32 → I32 → B
(≡) = coerce eqInt32
(≠) = coerce neInt32
(<) = coerce ltInt32
(≤) = coerce leInt32
(>) = coerce gtInt32
(≥) = coerce geInt32
{-# inline (≡) #-}
{-# inline (≠) #-}
{-# inline (>) #-}
{-# inline (≥) #-}
{-# inline (<) #-}
{-# inline (≤) #-}
