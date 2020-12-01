--------------------------------------------------------------------
-- | Description : 32-bit Signed Integer operations
--------------------------------------------------------------------
module I32 (I32(I32#,I32), module I32) where
import I ()
import I8 (I8(..))


deriving newtype instance (≡) I32
deriving newtype instance (≤) I32
instance ℕ I32 where
  (I32 x) + (I32 y) = I32 (x +# y)
  (I32 x) × (I32 y) = I32 (x *# y)
  (I32 x) / (I32 y) = I32 (divInt# x y)
  (I32 x) % (I32 y) = I32 (modInt# x y)
  (I32 x) /% (I32 y) = case x //%% y of (# d, m #) → (# I32 d, I32 m #)
instance ℤ I32 where
  negate (I32 x) = I32 (negateInt# x)
  (I32 x) - (I32 y) = I32 (x -# y)
  I32 x // I32 y = I32 (quotInt# x y)
  I32 x %% I32 y = I32 (remInt# x y)
  I32 x //%% I32 y = case quotRemInt# x y of (# q, r #) → (# I32 q, I32 r #)
-- | /Warning/: Bitwise operations rarely make sense on signed ints,
-- Consider using 'U' instead.
instance 𝔹 I32 where
  (∧) = coerce ((∧) @_ @I)
  (∨) = coerce ((∨) @_ @I)
  (⊕) = coerce ((⊕) @_ @I)
  (¬) (I32 u) = I32 (u ¬)

(+), (-), (×) ∷ I32 → I32 → I32
(I32 x) + (I32 y) = I32 (x +# y)
(I32 x) - (I32 y) = I32 (x -# y)
(I32 x) × (I32 y) = I32 (x *# y)
add, sub, mul, quot, rem ∷ I32 → I32 → I32
add (I32 y) (I32 x) = I32 (x +# y)
sub (I32 y) (I32 x) = I32 (x -# y)
mul (I32 y) (I32 x) = I32 (x *# y)
quot (I32 y) (I32 x) = I32 (quotInt# x y)
rem (I32 y) (I32 x) = I32 (remInt# x y)

quotRem ∷ I32 → I32 → (# I32, I32 #)
quotRem (I32 y) (I32 x) = case quotRemInt# x y of
  (# q, r #) → (# I32 q, I32 r #)

shiftL# ∷ I → I32 → I32
shiftL# i (I32 x) = I32 (uncheckedIShiftL# x i)

pattern Max, Min ∷ I32
pattern Max =  I32# 0x7FFFFFFF#
pattern Min = I32# -0x80000000#
