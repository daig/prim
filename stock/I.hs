module I (module I, module X) where
import B as X (B)

type I = Int

(≡), (≠), (<), (≤), (>), (≥) ∷ I → I → B
(≡) = coerce eqInt
(≠) = coerce neInt
(<) = coerce ltInt
(≤) = coerce leInt
(>) = coerce gtInt
(≥) = coerce geInt
{-# inline (≡) #-}
{-# inline (≠) #-}
{-# inline (>) #-}
{-# inline (≥) #-}
{-# inline (<) #-}
{-# inline (≤) #-}
cmp ∷ I → I → Ordering
cmp = coerce compareInt
{-# inline cmp #-}
