--------------------------------------------------------------------
-- | Description : 16-bit Signed Integer operations
--------------------------------------------------------------------
module Prim.I16 (I16(I16,I16), module Prim.I16) where
import Prim.I ()

newtype U16 ∷ T_U where U16# ∷ U → U16
-- | Narrow a machine 'U' to 16 bits
pattern U16 ∷ U → U16
pattern U16 i ← (coerce → i) where U16 = coerce narrow16Word#
{-# complete U16 #-}

newtype I16  ∷ T_I where I16 ∷ I → I16
-- | Narrow a machine 'I' to 16 bits
pattern I16 ∷ I → I16
pattern I16 i ← (coerce → i) where I16 = coerce narrow16Int#
{-# complete I16 #-}

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
pattern Max =  I16 0x7FFF#
pattern Min = I16 -0x800#
