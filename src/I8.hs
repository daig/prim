--------------------------------------------------------------------
-- | Description : 8-bit Signed Integer operations
--------------------------------------------------------------------
{-# language CPP #-}
{-# language ForeignFunctionInterface, CApiFFI, UnliftedFFITypes, GHCForeignImportPrim #-}
module I8 (I8(I8#,I8), module I8) where
import I ()
import Stock.Int


deriving newtype instance (≡) I8
deriving newtype instance (≤) I8
instance 𝔹 I8 where
  (∧) = coerce ((∧) @_ @I)
  (∨) = coerce ((∨) @_ @I)
  (⊕) = coerce ((⊕) @_ @I)
  (¬) (I8 u) = I8 (u ¬)

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
--pattern Max =  I8# INT_MAX#
pattern Max =  I8# 0x7F#
pattern Min = I8# -0x80#

