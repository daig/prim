module F32 (module F32
           ,Float,B) where

newtype F32 = F32# Float

(≡) ∷ F32 → F32 → B
(≡) = coerce eqFloat
{-# inline (≡) #-}
