module U16 (module U16, module X) where
import GHC.Word
import B as X (B)

newtype U16 = U16# Word16

(≡), (≠), (<), (≤), (>), (≥) ∷ U16 → U16 → B
(≡) = coerce eqWord16
(≠) = coerce neWord16
(<) = coerce ltWord16
(≤) = coerce leWord16
(>) = coerce gtWord16
(≥) = coerce geWord16
{-# inline (≡) #-}
{-# inline (≠) #-}
{-# inline (>) #-}
{-# inline (≥) #-}
{-# inline (<) #-}
{-# inline (≤) #-}
