module U32 (module U32, module X) where
import GHC.Word
import B as X (B)

newtype U32 = U32# Word32

(≡), (≠), (<), (≤), (>), (≥) ∷ U32 → U32 → B
(≡) = coerce eqWord32
(≠) = coerce neWord32
(<) = coerce ltWord32
(≤) = coerce leWord32
(>) = coerce gtWord32
(≥) = coerce geWord32
{-# inline (≡) #-}
{-# inline (≠) #-}
{-# inline (>) #-}
{-# inline (≥) #-}
{-# inline (<) #-}
{-# inline (≤) #-}
