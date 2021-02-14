module U64 (module U64, module X) where
import GHC.Word as X (Word64)
import B as X (B)

newtype U64 = U64 Word64

(≡), (≠), (<), (≤), (>), (≥) ∷ U64 → U64 → B
(≡) = coerce eqWord64
(≠) = coerce neWord64
(<) = coerce ltWord64
(≤) = coerce leWord64
(>) = coerce gtWord64
(≥) = coerce geWord64
{-# inline (≡) #-}
{-# inline (≠) #-}
{-# inline (>) #-}
{-# inline (≥) #-}
{-# inline (<) #-}
{-# inline (≤) #-}
