--------------------------------------------------------------------
-- | Description : 8-bit Signed Integer operations
--------------------------------------------------------------------
module I8 (I8(I8#,I8), module I8) where
import I ()

deriving newtype instance (≡) I8
deriving newtype instance (≤) I8
deriving newtype instance (⊕) I8
instance (¬) I8 where (¬) (I8 u) = I8 (u ¬)

(+), (-), (×) ∷ I8 → I8 → I8
(I8 x) + (I8 y) = I8 (x +# y)
(I8 x) - (I8 y) = I8 (x -# y)
(I8 x) × (I8 y) = I8 (x *# y)
I8 x // I8 y = I8 (quotInt# x y)
I8 x %% I8 y = I8 (remInt# x y)
(//%%) ∷ I8 → I8 → (# I8, I8 #)
I8 x //%% I8 y = case quotRemInt# x y of (# q, r #) → (# I8 q, I8 r #)

shiftL# ∷ I → I8 → I8
shiftL# i (I8 x) = I8 (uncheckedIShiftL# x i)

pattern Max, Min ∷ I8
pattern Max =  I8# 0x7F#
pattern Min = I8# -0x80#
