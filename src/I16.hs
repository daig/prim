--------------------------------------------------------------------
-- | Description : 16-bit Signed Integer operations
--------------------------------------------------------------------
module I16 (I16(I16#,I16), module I16) where
import I ()

deriving newtype instance (≡) I16
deriving newtype instance (≤) I16
instance 𝔹 I16 where
  (∧) = coerce ((∧) @_ @I)
  (∨) = coerce ((∨) @_ @I)
  (⊕) = coerce ((⊕) @_ @I)
  (¬) (I16 u) = I16 (u ¬)

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
