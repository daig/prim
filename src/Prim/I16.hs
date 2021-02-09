--------------------------------------------------------------------
-- | Description : 16-bit Signed Integer operations
--------------------------------------------------------------------
module Prim.I16 (I16(I16#,I16), module Prim.I16) where
import Prim.I ()

deriving newtype instance (≡) I16
deriving newtype instance (≤) I16

instance ℕ I16 where
  (I16 x) + (I16 y) = I16 (x +# y)
  (I16 x) × (I16 y) = I16 (x *# y)
  (I16 x) / (I16 y) = I16 (divInt# x y)
  (I16 x) % (I16 y) = I16 (modInt# x y)
  (I16 x) /% (I16 y) = case x //%% y of (# d, m #) → (# I16 d, I16 m #)
  addC (I16 a) (I16 b) = let c = a + b in (# I16 c , c > coerce Max #)
  subC (I16 a) (I16 b) = let c = a - b in (# I16 c , c < coerce Min #)
instance ℤ I16 where
  negate (I16 x) = I16 (negateInt# x)
  (I16 x) - (I16 y) = I16 (x -# y)
  I16 x // I16 y = I16 (quotInt# x y)
  I16 x %% I16 y = I16 (remInt# x y)
  I16 x //%% I16 y = case quotRemInt# x y of (# q, r #) → (# I16 q, I16 r #)

pattern Max, Min ∷ I16
pattern Max =  I16# 0x7FFF#
pattern Min = I16# -0x800#
