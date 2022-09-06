module F32 where

decode ∷ F32 → (# I {- 32 bit -}, I {- 32 bit -} #)
decode = coerce decodeFloat_Int#

{-
pattern Inf ∷ F32
pattern Inf ← (((1.0# / 0.0#) ≡) → T) where Inf = 1.0# / 0.0#
pattern Inf_ ∷ F32
pattern Inf_ ← (((-1.0# / 0.0#) ≡) → T) where Inf_ = -1.0# / 0.0#
-}
