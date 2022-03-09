module U (module U, module X) where
import B as X (B)

newtype U = U Word

(≡), (≠), (<), (≤), (>), (≥) ∷ U → U → B
(≡) = coerce eqWord
(≠) = coerce neWord
(<) = coerce ltWord
(≤) = coerce leWord
(>) = coerce gtWord
(≥) = coerce geWord
{-# inline (≡) #-}
{-# inline (≠) #-}
{-# inline (>) #-}
{-# inline (≥) #-}
{-# inline (<) #-}
{-# inline (≤) #-}
cmp ∷ U → U → Ordering
cmp = coerce compareWord
{-# inline cmp #-}
