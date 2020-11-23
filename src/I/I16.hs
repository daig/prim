module I.I16 (I16(I16#,I16), module I.I16) where

-- | Narrow a machine 'I' to 16 bits
pattern I16 ∷ I → I16
pattern I16 i ← (coerce narrow16Int# → i) where I16 = coerce
{-# complete I16 #-}

(+), (-), (×) ∷ I16 → I16 → I16
(I16 x) + (I16 y) = I16 (x +# y)
(I16 x) - (I16 y) = I16 (x -# y)
(I16 x) × (I16 y) = I16 (x *# y)
I16 x // I16 y = I16 (quotInt# x y)
I16 x %% I16 y = I16 (remInt# x y)
(//%%) ∷ I16 → I16 → (# I16, I16 #)
I16 x //%% I16 y = case quotRemInt# x y of (# q, r #) → (# I16 q, I16 r #)

shiftL# ∷ I → I16 → I16
shiftL# i (I16 x) = I16 (uncheckedIShiftL# x i)

pattern Max, Min ∷ I16
pattern Max =  I16# 0x7FFF#
pattern Min = I16# -0x800#
