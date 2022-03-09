module I64 (module I64, module X) where
import GHC.Int
import B as X (B)

newtype I64 = I64 Int64

(≡), (≠), (<), (≤), (>), (≥) ∷ I64 → I64 → B
(≡) = coerce eqInt64
(≠) = coerce neInt64
(<) = coerce ltInt64
(≤) = coerce leInt64
(>) = coerce gtInt64
(≥) = coerce geInt64
{-# inline (≡) #-}
{-# inline (≠) #-}
{-# inline (>) #-}
{-# inline (≥) #-}
{-# inline (<) #-}
{-# inline (≤) #-}
