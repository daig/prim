module U8 (module U8, module X) where
import GHC.Word
import B as X (B)

newtype U8 = U8# Word8

(≡), (≠), (<), (≤), (>), (≥) ∷ U8 → U8 → B
(≡) = coerce eqWord8
(≠) = coerce neWord8
(<) = coerce ltWord8
(≤) = coerce leWord8
(>) = coerce gtWord8
(≥) = coerce geWord8
{-# inline (≡) #-}
{-# inline (≠) #-}
{-# inline (>) #-}
{-# inline (≥) #-}
{-# inline (<) #-}
{-# inline (≤) #-}
