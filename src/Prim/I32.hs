--------------------------------------------------------------------
-- | Description : 32-bit Signed Integer operations
--------------------------------------------------------------------
module Prim.I32 (I32(I32#,I32), module Prim.I32) where
import Prim.I ()
import Prim.I8 (I8(..))


deriving newtype instance (≡) I32
deriving newtype instance (≤) I32
instance ℕ I32 where
  (I32 x) + (I32 y) = I32 (x +# y)
  (I32 x) × (I32 y) = I32 (x *# y)
  (I32 x) / (I32 y) = I32 (divInt# x y)
  (I32 x) % (I32 y) = I32 (modInt# x y)
  (I32 x) /% (I32 y) = case x //%% y of (# d, m #) → (# I32 d, I32 m #)
  addC (I32 a) (I32 b) = let c = a + b in (# I32 c , c > coerce Max #)
  subC (I32 a) (I32 b) = let c = a - b in (# I32 c , c < coerce Min #)
instance ℤ I32 where
  negate (I32 x) = I32 (negateInt# x)
  (I32 x) - (I32 y) = I32 (x -# y)
  I32 x // I32 y = I32 (quotInt# x y)
  I32 x %% I32 y = I32 (remInt# x y)
  I32 x //%% I32 y = case quotRemInt# x y of (# q, r #) → (# I32 q, I32 r #)

pattern Max, Min ∷ I32
pattern Max =  I32# 0x7FFFFFFF#
pattern Min = I32# -0x80000000#
