module I16 (module I16, module X) where
import GHC.Int
import B as X (B)

newtype I16 = I16 Int16

(≡), (≠), (<), (≤), (>), (≥) ∷ I16 → I16 → B
(≡) = coerce eqInt16
(≠) = coerce neInt16
(<) = coerce ltInt16
(≤) = coerce leInt16
(>) = coerce gtInt16
(≥) = coerce geInt16
{-# inline (≡) #-}
{-# inline (≠) #-}
{-# inline (>) #-}
{-# inline (≥) #-}
{-# inline (<) #-}
{-# inline (≤) #-}
